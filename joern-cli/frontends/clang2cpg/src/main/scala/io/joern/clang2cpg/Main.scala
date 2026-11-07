package io.joern.clang2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgFrontend, X2CpgMain}
import io.joern.clang2cpg.parser.clang.ClangCpg
import io.shiftleft.codepropertygraph.generated.Cpg
import scopt.OParser
import scala.util.Try

final case class Config(
  parserBackend: String = "",
  clangCommand: Option[String] = None,
  clangPlugin: Option[String] = None,
  clangEvidenceDir: Option[String] = None,
  printIfDefsOnly: Boolean = false,
  override val genericConfig: X2CpgConfig.GenericConfig = X2CpgConfig.GenericConfig()
) extends X2CpgConfig[Config] {
  override def withGenericConfig(value: X2CpgConfig.GenericConfig): Config = copy(genericConfig = value)
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
      opt[String]("clang-command")
        .text("JSON argv with the exact compiler, build flags and one source file")
        .action((value, config) => config.copy(clangCommand = Some(value))),
      opt[String]("clang-plugin")
        .text("Matching native Clang exporter shared library")
        .action((value, config) => config.copy(clangPlugin = Some(value))),
      opt[String]("clang-evidence-dir")
        .text("Fresh directory for compiler provenance")
        .action((value, config) => config.copy(clangEvidenceDir = Some(value))),
      opt[Unit]("print-ifdef-only")
        .action((_, config) => config.copy(printIfDefsOnly = true)),
      checkConfig(config =>
        if (config.printIfDefsOnly) failure("Clang preprocessor-only mode is not implemented")
        else if (config.parserBackend != "clang") failure("replacement frontend requires explicit --parser clang")
        else success
      )
    )
  }
}

class Clang2Cpg extends X2CpgFrontend {
  final override type ConfigType = Config
  override val defaultConfig: Config = Config()
  override def createCpg(config: Config): Try[Cpg] = Try {
    require(config.parserBackend == "clang", "replacement frontend requires explicit --parser clang")
    require(!config.printIfDefsOnly, "Clang preprocessor-only mode is not implemented")
    config
  }.flatMap(ClangCpg.create)

  def printIfDefsOnly(config: Config): Unit = {
    throw new IllegalArgumentException("Clang preprocessor-only mode is not implemented")
  }
}

object Main extends X2CpgMain(new Clang2Cpg(), Frontend.cmdLineParser)
