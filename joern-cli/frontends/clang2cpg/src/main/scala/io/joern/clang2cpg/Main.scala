package io.joern.clang2cpg

import io.joern.x2cpg.{SourceFiles, X2CpgConfig, X2CpgFrontend, X2CpgMain}
import io.joern.clang2cpg.parser.ClangInputs
import io.joern.clang2cpg.parser.clang.ClangCpg
import io.shiftleft.codepropertygraph.generated.Cpg
import scopt.OParser
import scala.util.Try

final case class Config(
  parserBackend: String = "clang",
  clangCommand: Option[String] = None,
  clangWorkers: Int = 1,
  embedNativeEvidence: Boolean = true,
  clangCompiler: Option[String] = None,
  clangPlugin: Option[String] = None,
  clangEvidenceDir: Option[String] = None,
  includePaths: Set[String] = Set.empty,
  defines: Set[String] = Set.empty,
  includeComments: Boolean = false,
  logProblems: Boolean = false,
  logPreprocessor: Boolean = false,
  printIfDefsOnly: Boolean = false,
  includePathsAutoDiscovery: Boolean = false,
  skipFunctionBodies: Boolean = false,
  withPreprocessedFiles: Boolean = false,
  compilationDatabaseFilename: Option[String] = None,
  legacyNoImageLocations: Boolean = false,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)
  def withIncludePaths(value: Set[String]): Config = copy(includePaths = value)
  def withDefines(value: Set[String]): Config = copy(defines = value)
  def withIncludeComments(value: Boolean): Config = copy(includeComments = value)
  def withLogProblems(value: Boolean): Config = copy(logProblems = value)
  def withLogPreprocessor(value: Boolean): Config = copy(logPreprocessor = value)
  def withPrintIfDefsOnly(value: Boolean): Config = copy(printIfDefsOnly = value)
  def withIncludePathsAutoDiscovery(value: Boolean): Config = copy(includePathsAutoDiscovery = value)
  def withSkipFunctionBodies(value: Boolean): Config = copy(skipFunctionBodies = value)
  def withPreprocessedFiles(value: Boolean): Config = copy(withPreprocessedFiles = value)
  def withCompilationDatabase(value: String): Config = copy(compilationDatabaseFilename = Some(value))

  def validateBackend(): Unit = {
    require(parserBackend == "clang", "clang2cpg supports only the Clang backend; no CDT substitution is permitted")
  }

  def reportCompatibilityOptions(): Unit = {
    if (legacyNoImageLocations) {
      System.err.println(
        "--no-image-locations is deprecated; stock c2cpg and clang2cpg retain source locations"
      )
    }
  }
}

object Frontend {
  implicit val defaultConfig: Config = Config()
  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder.*
    OParser.sequence(
      programName("Clang2Cpg"),
      opt[String]("parser")
        .validate(value => if (value == "clang") success else failure("clang2cpg supports only --parser clang"))
        .action((value, config) => config.copy(parserBackend = value)),
      opt[Unit]("clang-no-raw-payload")
        .text("Emit the complete semantic CPG without the redundant legacy compiler graph/token/serialized-AST payload")
        .action((_, config) => config.copy(embedNativeEvidence = false)),
      opt[Int]("clang-workers")
        .validate(value => if (value > 0) success else failure("--clang-workers must be positive"))
        .action((value, config) => config.copy(clangWorkers = value)),
      opt[String]("clang-command")
        .text("JSON argv with the exact compiler, build flags and one source file")
        .action((value, config) => config.copy(clangCommand = Some(value))),
      opt[String]("clang-compiler")
        .text("Explicit Clang executable for source discovery or compilation database commands")
        .action((value, config) => config.copy(clangCompiler = Some(value))),
      opt[String]("clang-plugin")
        .text("Matching native Clang exporter shared library")
        .action((value, config) => config.copy(clangPlugin = Some(value))),
      opt[String]("clang-evidence-dir")
        .text("Fresh directory for compiler provenance")
        .action((value, config) => config.copy(clangEvidenceDir = Some(value))),
      opt[Unit]("include-comments")
        .text("Include compiler-recorded comments in the CPG")
        .action((_, config) => config.withIncludeComments(true)),
      opt[Unit]("log-problems")
        .text("Print compiler diagnostics while retaining diagnostic evidence")
        .action((_, config) => config.withLogProblems(true)),
      opt[Unit]("log-preprocessor")
        .text("Print compiler-recorded preprocessing events")
        .action((_, config) => config.withLogPreprocessor(true)),
      opt[Unit]("print-ifdef-only")
        .text("Print conditional preprocessing statements without creating a CPG")
        .action((_, config) => config.withPrintIfDefsOnly(true)),
      opt[String]("include")
        .unbounded()
        .text("Header include search path")
        .action((value, config) => config.withIncludePaths(config.includePaths + value)),
      opt[String]("define")
        .unbounded()
        .text("Preprocessor definition")
        .action((value, config) => config.withDefines(config.defines + value)),
      opt[Unit]("with-include-auto-discovery")
        .text("Use the selected compiler's system include search discovery")
        .action((_, config) => config.withIncludePathsAutoDiscovery(true)),
      opt[Unit]("no-include-auto-discovery")
        .hidden()
        .action((_, config) => config.withIncludePathsAutoDiscovery(false)),
      opt[Unit]("skip-function-bodies")
        .text("Emit declarations without source function bodies")
        .action((_, config) => config.withSkipFunctionBodies(true)),
      opt[Unit]("with-preprocessed-files")
        .text("Include preprocessed .i files and prefer them to corresponding source files")
        .action((_, config) => config.withPreprocessedFiles(true)),
      opt[Unit]("no-image-locations")
        .hidden()
        .action((_, config) => config.copy(legacyNoImageLocations = true)),
      opt[String]("compilation-database")
        .text("Compilation database JSON or directory containing compile_commands.json")
        .action((value, config) =>
          config.withCompilationDatabase(SourceFiles.toAbsolutePath(value, config.inputPath))
        )
    )
  }
}

class Clang2Cpg extends X2CpgFrontend {
  final override type ConfigType = Config
  override val defaultConfig: Config = Config()
  override def createCpg(config: Config): Try[Cpg] = Try {
    config.validateBackend()
    require(!config.printIfDefsOnly, "preprocessor-only mode must use printIfDefsOnly instead of createCpg")
    config.reportCompatibilityOptions()
    config
  }.flatMap(ClangCpg.create)

  def printIfDefsOnly(config: Config): Unit = {
    config.validateBackend()
    config.reportCompatibilityOptions()
    ClangInputs.printIfDefsOnly(config)
  }
}

object Main extends X2CpgMain(new Clang2Cpg(), Frontend.cmdLineParser) {
  override def run(config: frontend.ConfigType): Unit = {
    config match {
      case selected: Config if selected.printIfDefsOnly =>
        frontend.asInstanceOf[Clang2Cpg].printIfDefsOnly(selected)
      case _ => super.run(config)
    }
  }
}
