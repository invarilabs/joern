package io.joern.clang2cpg.parser

import io.joern.clang2cpg.Config
import java.nio.file.{Files, Path, Paths, FileVisitOption, FileVisitResult, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

final case class ClangInput(source: Path, command: Seq[String], directory: Path, protectedInputs: Seq[Path] = Seq.empty)

/** Selects translation units and their actual build context; never executes shell commands. */
object ClangInputs {
  private val sources = Set("c", "c++", "cc", "cxx", "cpp", "cp", "ccm", "cxxm", "c++m")
  private val headers = Set("h", "hpp", "hh", "hp", "hxx", "h++", "tcc")
  private def extension(path: Path): String = path.getFileName.toString.split("\\.").last.toLowerCase
  private def absolute(value: String, cwd: Path): Path = {
    val path = Paths.get(value)
    (if (path.isAbsolute) path else cwd.resolve(path)).toAbsolutePath.normalize()
  }
  private def existing(value: String, cwd: Path): Path = absolute(value, cwd).toRealPath()
  private def compiler(value: String, cwd: Path): Path = {
    val path = Paths.get(value)
    val result = if (path.isAbsolute || value.contains("/")) absolute(value, cwd) else {
      val matches = sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).toSeq
        .map(part => absolute(part, cwd).resolve(value)).filter(p => Files.isRegularFile(p) && Files.isExecutable(p))
      require(matches.nonEmpty, s"Explicit compiler $value is unavailable on PATH; configure --clang-compiler")
      matches.head
    }
    require(Files.isRegularFile(result) && Files.isExecutable(result), s"Compiler is missing or not executable: $result")
    result.toAbsolutePath.normalize()
  }

  /** POSIX quoting without shell expansion, operators, substitutions or execution. */
  def commandWords(text: String): Seq[String] = {
    val out = mutable.ArrayBuffer.empty[String]
    val word = new StringBuilder
    var quote = ' '
    var escaped = false
    var active = false
    text.foreach { c =>
      if (escaped) { word.append(c); escaped = false; active = true }
      else if (c == '\\' && quote != '\'') { escaped = true; active = true }
      else if (quote != ' ') {
        if (c == quote) quote = ' '
        else {
          require(quote == '\'' || (c != '$' && c != '`'), "Shell expansion is unsupported in compiler commands; use arguments JSON")
          word.append(c)
        }
      } else if (c == '\'' || c == '"') { quote = c; active = true }
      else if (c.isWhitespace) {
        require(c != '\n' && c != '\r', "Shell command separators are unsupported; use arguments JSON")
        if (active) { out += word.toString; word.clear(); active = false }
      } else {
        require(!";|&<>$`".contains(c), "Shell operators/expansion are unsupported; use arguments JSON")
        word.append(c); active = true
      }
    }
    require(!escaped && quote == ' ', "Unterminated quote or escape in compiler command")
    if (active) out += word.toString
    require(out.nonEmpty, "Compiler command is empty")
    out.toSeq
  }

  private def expand(words: Seq[String], cwd: Path, active: Set[Path], files: mutable.ArrayBuffer[Path]): Seq[String] =
    words.flatMap { word =>
      if (!word.startsWith("@")) Seq(word)
      else {
        val path = existing(word.drop(1), cwd)
        require(!active(path), s"Compiler response-file cycle: $path")
        files += path
        expand(commandWords(Files.readString(path)), cwd, active + path, files)
      }
    }

  private def buildCommand(words: Seq[String], source: Path, cwd: Path, selectedCompiler: Option[String]): Seq[String] = {
    require(words.nonEmpty, s"Empty compiler command for $source")
    val out = mutable.ArrayBuffer(compiler(selectedCompiler.getOrElse(words.head), cwd).toString)
    val pairedPaths = Set("-I", "-isystem", "-iquote", "-idirafter", "-include", "-imacros", "-isysroot", "--sysroot", "-F", "-iframework")
    val discardedPairs = Set("-o", "-MF", "-MT", "-MQ", "-MJ")
    val discarded = Set("-c", "-S", "-MD", "-MMD", "-MP")
    var index = 1
    var occurrences = 0
    while (index < words.size) {
      val word = words(index)
      if (pairedPaths(word) || discardedPairs(word)) {
        require(index + 1 < words.size, s"Missing argument after $word in command for $source")
        if (pairedPaths(word)) { out += word; out += absolute(words(index + 1), cwd).toString }
        index += 2
      } else if (discarded(word)) index += 1
      else if (word.startsWith("-I") && word.length > 2) { out += "-I" + absolute(word.drop(2), cwd); index += 1 }
      else if (Seq("-isystem", "-iquote", "-idirafter", "-include", "-imacros", "-isysroot", "-iframework").exists(prefix => word.startsWith(prefix) && word.length > prefix.length)) {
        val prefix = Seq("-isystem", "-iquote", "-idirafter", "-include", "-imacros", "-isysroot", "-iframework").find(word.startsWith).get
        out += prefix + absolute(word.drop(prefix.length), cwd); index += 1
      }
      else if (word.startsWith("--sysroot=")) { out += "--sysroot=" + absolute(word.drop(10), cwd); index += 1 }
      else if (Seq("-MF", "-MT", "-MQ", "-MJ").exists(prefix => word.startsWith(prefix) && word.length > prefix.length)) index += 1
      else if (!word.startsWith("-") && Files.isRegularFile(absolute(word, cwd)) && existing(word, cwd) == source) { occurrences += 1; index += 1 }
      else {
        require(!Set("-emit-ast", "-fsyntax-only", "-M", "-MM")(word), s"Incompatible capture flag $word for $source")
        out += word; index += 1
      }
    }
    require(occurrences == 1, s"Compiler command must select $source exactly once (found $occurrences)")
    out.toSeq :+ source.toString
  }

  def plan(config: Config): Seq[ClangInput] = {
    val input = Paths.get(config.inputPath).toRealPath()
    val processCwd = Paths.get("").toAbsolutePath.normalize()
    require(Files.isRegularFile(input) || Files.isDirectory(input), s"Input is not a source file or directory: $input")
    val root = if (Files.isDirectory(input)) input else input.getParent
    require(!(config.clangCommand.nonEmpty && config.compilationDatabaseFilename.nonEmpty), "Ambiguous compiler context: select --clang-command or --compilation-database")
    def excluded(path: Path, directory: Boolean = false): Boolean = {
      val relative = if (path == root) path.getFileName.toString else root.relativize(path).toString
      val parts = Paths.get(relative).iterator().asScala.map(_.toString).toSeq
      val directories = if (directory) parts else parts.dropRight(1)
      def matches(pattern: scala.util.matching.Regex): Boolean =
        pattern.matches(relative) || (directory && pattern.matches(relative + java.io.File.separator))
      parts.exists(_.startsWith(".")) || directories.exists(p => Set("test", "tests", "CMakeFiles")(p)) ||
      config.ignoredFiles.exists(name => path.startsWith(absolute(name, root))) ||
      matches(config.ignoredFilesRegex) || config.defaultIgnoredFilesRegex.exists(matches)
    }
    // Database mode selects its declared translation units, not unrelated files found by walking the repository.
    val discovered = if (Files.isRegularFile(input)) Seq(input) else if (config.compilationDatabaseFilename.nonEmpty) Seq.empty else {
      val files = mutable.ArrayBuffer.empty[Path]
      Files.walkFileTree(input, java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS), Int.MaxValue,
        new SimpleFileVisitor[Path] {
          override def preVisitDirectory(path: Path, attributes: BasicFileAttributes): FileVisitResult =
            if (excluded(path, directory = true)) FileVisitResult.SKIP_SUBTREE else FileVisitResult.CONTINUE
          override def visitFile(path: Path, attributes: BasicFileAttributes): FileVisitResult = {
            if (attributes.isRegularFile && !excluded(path)) files += path.toRealPath()
            FileVisitResult.CONTINUE
          }
          // SimpleFileVisitor throws traversal failures, including symlink cycles; no failed file is omitted.
        })
      files.toSeq
    }
    val candidates = discovered.distinct.filterNot(path => excluded(path)).filter { p =>
      sources(extension(p)) || headers(extension(p)) || (config.withPreprocessedFiles && extension(p) == "i")
    }
    val preprocessed = candidates.filter(path => extension(path) == "i").toSet
    val selected = candidates.filterNot { path =>
      val sibling = path.resolveSibling(path.getFileName.toString.replaceFirst("\\.[^.]+$", ".i"))
      Files.isDirectory(input) && config.compilationDatabaseFilename.isEmpty && config.withPreprocessedFiles &&
        extension(path) != "i" && preprocessed(sibling)
    }.sortBy(_.toString)
    if (config.compilationDatabaseFilename.isEmpty)
      require(selected.nonEmpty, s"No selected C/C++ source or header files in $input; correct the input/exclusions")
    config.clangCommand match {
      case Some(name) =>
        require(Files.isRegularFile(input), "--clang-command requires one explicitly selected file")
        require(config.includePaths.isEmpty && config.defines.isEmpty,
          "--clang-command already defines the exact argv; put include/define options in that JSON")
        val file = existing(name, processCwd)
        val words = ujson.read(Files.readString(file)).arr.toSeq.map(_.str)
        require(words.size >= 2 && words.last == input.toString, "Clang command must end with the exact selected absolute source path")
        require(Paths.get(words.head).isAbsolute, "Clang command requires an explicit absolute compiler executable")
        compiler(words.head, root)
        Seq(ClangInput(input, words, root, Seq(file)))
      case None => config.compilationDatabaseFilename match {
        case Some(name) =>
          val database = existing(name, processCwd)
          val entries = ujson.read(Files.readString(database)).arr.toSeq.map { entry =>
            val cwd = existing(entry("directory").str, database.getParent)
            require(Files.isDirectory(cwd), s"Compilation database working directory is missing: $cwd")
            val source = existing(entry("file").str, cwd)
            val hasArgs = entry.obj.contains("arguments")
            val hasCommand = entry.obj.contains("command")
            require(hasArgs || hasCommand, s"Compilation database entry has no command: $source")
            val words = if (hasArgs) entry("arguments").arr.toSeq.map(_.str) else commandWords(entry("command").str)
            (source, cwd, words)
          }.groupBy(_._1)
          // A compilation database defines its translation units; included headers are captured in their closure.
          val translationUnits = if (Files.isDirectory(input))
            entries.keys.toSeq.filter(_.startsWith(input)).filterNot(path => excluded(path)).sortBy(_.toString)
          else {
            require(!excluded(input), s"Explicit translation unit is excluded: $input; correct the exclusions")
            Seq(input)
          }
          require(translationUnits.nonEmpty, s"Compilation database has no selected translation units under $input")
          translationUnits.map { source =>
            val matches = entries.getOrElse(source, throw new IllegalArgumentException(s"Compilation database assignment missing for $source; provide its compiler context"))
            require(matches.size == 1, s"Compilation database assignment ambiguous for $source (${matches.size} entries)")
            val (_, cwd, words) = matches.head
            val responses = mutable.ArrayBuffer.empty[Path]
            val expanded = expand(words, cwd, Set.empty, responses)
            val command = buildCommand(expanded, source, cwd, config.clangCompiler.map(value => compiler(value, processCwd).toString))
            val explicitFlags = config.includePaths.toSeq.sorted.map(path => "-I" + absolute(path, processCwd)) ++ config.defines.toSeq.sorted.map("-D" + _)
            ClangInput(source, command.dropRight(1) ++ explicitFlags :+ source.toString, cwd, Seq(database) ++ responses)
          }
        case None =>
          val tool = config.clangCompiler.orElse(sys.env.get("INVARI_C_COMPILER")).getOrElse(
            throw new IllegalArgumentException("Clang compiler is unspecified; configure --clang-compiler or INVARI_C_COMPILER"))
          val executable = compiler(tool, processCwd).toString
          val flags = config.includePaths.toSeq.sorted.map(path => "-I" + absolute(path, processCwd)) ++ config.defines.toSeq.sorted.map("-D" + _)
          selected.map(p => ClangInput(p, Seq(executable) ++ flags :+ p.toString, root))
      }
    }
  }

  def plugin(config: Config): Path = {
    val path = Paths.get(config.clangPlugin.orElse(sys.env.get("INVARI_CLANG_CPG_PLUGIN")).getOrElse(
      throw new IllegalArgumentException("Clang exporter is unspecified; configure --clang-plugin or INVARI_CLANG_CPG_PLUGIN"))).toAbsolutePath.normalize()
    require(Files.isRegularFile(path), s"Clang exporter is missing: $path")
    path
  }
  def evidenceDirectory(config: Config): Path = Paths.get(config.clangEvidenceDir.getOrElse(config.outputPath + ".clang-evidence")).toAbsolutePath.normalize()

  def printIfDefsOnly(config: Config): Unit = {
    val inputs = plan(config)
    val library = plugin(config)
    val directory = evidenceDirectory(config)
    require(!Files.exists(directory), s"Clang evidence path already exists: $directory; select a fresh directory")
    val conditions = inputs.zipWithIndex.flatMap { case (input, index) =>
      val evidence = ClangParser.capture(input, library, directory.resolve(f"$index%06d"), config)
      require(evidence.document.obj.contains("conditional_directives"), "Clang exporter lacks conditional directive events; rebuild matching exporter")
      evidence.document("conditional_directives").arr.toSeq.map(_("condition").str)
    }.distinct.sorted
    println(conditions.mkString(","))
  }
}
