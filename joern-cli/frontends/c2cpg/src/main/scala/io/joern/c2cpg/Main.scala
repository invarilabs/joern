package io.joern.c2cpg

import io.joern.c2cpg.Frontend.*
import io.joern.x2cpg.{SourceFiles, X2CpgConfig, X2CpgMain}
import scopt.OParser

final case class Config(
  includePaths: Set[String] = Set.empty,
  defines: Set[String] = Set.empty,
  includeComments: Boolean = false,
  logProblems: Boolean = false,
  logPreprocessor: Boolean = false,
  printIfDefsOnly: Boolean = false,
  includePathsAutoDiscovery: Boolean = false,
  skipFunctionBodies: Boolean = false,
  withPreprocessedFiles: Boolean = false,
  cTypeEvidence: Boolean = false,
  parserBackend: String = "cdt",
  clangCommand: Option[String] = None,
  clangPlugin: Option[String] = None,
  clangEvidenceDir: Option[String] = None,
  clangFactsCommand: Option[String] = None,
  compilationDatabaseFilename: Option[String] = None,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {

  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)

  def withIncludePaths(includePaths: Set[String]): Config = {
    this.copy(includePaths = includePaths)
  }

  def withDefines(defines: Set[String]): Config = {
    this.copy(defines = defines)
  }

  def withIncludeComments(value: Boolean): Config = {
    this.copy(includeComments = value)
  }

  def withLogProblems(value: Boolean): Config = {
    this.copy(logProblems = value)
  }

  def withLogPreprocessor(value: Boolean): Config = {
    this.copy(logPreprocessor = value)
  }

  def withPrintIfDefsOnly(value: Boolean): Config = {
    this.copy(printIfDefsOnly = value)
  }

  def withIncludePathsAutoDiscovery(value: Boolean): Config = {
    this.copy(includePathsAutoDiscovery = value)
  }

  def withSkipFunctionBodies(value: Boolean): Config = {
    this.copy(skipFunctionBodies = value)
  }

  def withPreprocessedFiles(value: Boolean): Config = {
    this.copy(withPreprocessedFiles = value)
  }

  def withCTypeEvidence(value: Boolean): Config = copy(cTypeEvidence = value)

  def withCompilationDatabase(value: String): Config = {
    this.copy(compilationDatabaseFilename = Option(value))
  }
}

object Frontend {
  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName(classOf[C2Cpg].getSimpleName),
      opt[String]("parser")
        .validate(value => if (Set("cdt", "clang")(value)) success else failure("parser must be cdt or clang"))
        .action((value, config) => config.copy(parserBackend = value)),
      opt[String]("clang-command")
        .text("JSON argv for one explicit Clang translation-unit command")
        .action((value, config) => config.copy(clangCommand = Some(value))),
      opt[String]("clang-plugin")
        .text("matching native Clang CPG exporter library")
        .action((value, config) => config.copy(clangPlugin = Some(value))),
      opt[String]("clang-facts-command")
        .text("Explicit JSON argv for the frontend completed-facts producer")
        .action((value, config) => config.copy(clangFactsCommand = Some(value))),
      opt[String]("clang-evidence-dir")
        .text("new directory retaining compiler and graph evidence")
        .action((value, config) => config.copy(clangEvidenceDir = Some(value))),
      opt[Unit]("include-comments")
        .text(s"includes all comments into the CPG")
        .action((_, c) => c.withIncludeComments(true)),
      opt[Unit]("log-problems")
        .text(s"enables logging of all parse problems while generating the CPG")
        .action((_, c) => c.withLogProblems(true)),
      opt[Unit]("log-preprocessor")
        .text(s"enables logging of all preprocessor statements while generating the CPG")
        .action((_, c) => c.withLogPreprocessor(true)),
      opt[Unit]("print-ifdef-only")
        .text(s"prints a comma-separated list of all preprocessor ifdef and if statements; does not create a CPG")
        .action((_, c) => c.withPrintIfDefsOnly(true)),
      opt[String]("include")
        .unbounded()
        .text("header include paths")
        .action((incl, c) => c.withIncludePaths(c.includePaths + incl)),
      opt[Unit]("no-include-auto-discovery")
        .text("disables auto discovery of system header include paths")
        .hidden(),
      opt[Unit]("with-include-auto-discovery")
        .text("enables auto discovery of system header include paths")
        .action((_, c) => c.withIncludePathsAutoDiscovery(true)),
      opt[Unit]("with-c-type-evidence")
        .text(
          "preserves CDT-backed C record/enum identities and field/index types; unresolved required evidence is an error"
        )
        .action((_, config) => config.withCTypeEvidence(true)),
      opt[Unit]("skip-function-bodies")
        .text("instructs the parser to skip function and method bodies.")
        .action((_, c) => c.withSkipFunctionBodies(true)),
      opt[Unit]("no-image-locations")
        // deprecated, won't be removed for now to avoid breaking existing scripts
        .hidden(),
      opt[Unit]("with-preprocessed-files")
        .text("includes *.i files and gives them priority over their unprocessed origin source files.")
        .action((_, c) => c.withPreprocessedFiles(true)),
      opt[String]("define")
        .unbounded()
        .text("define a name")
        .action((d, c) => c.withDefines(c.defines + d)),
      opt[String]("compilation-database")
        .text("""enables the processing of compilation database files (e.g., compile_commands.json).
            | This allows to automatically extract compiler options, source files, and other build information from the specified database
            | and ensuring consistency with the build configuration.
            | For a cmake based build such a file is generated with the environment variable CMAKE_EXPORT_COMPILE_COMMANDS being present.
            | Clang based build are supported e.g., with https://github.com/rizsotto/Bear
            | """.stripMargin)
        .action((d, c) => c.withCompilationDatabase(SourceFiles.toAbsolutePath(d, c.inputPath))),
      checkConfig(config =>
        if (config.parserBackend == "clang" && config.printIfDefsOnly)
          failure("Clang preprocessor-only mode is not implemented; no CDT substitution is permitted")
        else success
      )
    )
  }

}

object Main extends X2CpgMain(new C2Cpg(), cmdLineParser) {

  override def run(config: frontend.ConfigType): Unit = {
    config match {
      case c: Config if c.printIfDefsOnly => frontend.asInstanceOf[C2Cpg].printIfDefsOnly(c)
      case _                              => super.run(config)
    }
  }

}
