import sbt.*
import sbt.Keys.*

lazy val clang2cpg = (project in file("."))
  .settings(
    name := "c2cpg",
    version := "4.0.662",
    scalaVersion := "3.9.0",
    Compile / mainClass := Some("io.joern.c2cpg.Main"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Werror", "-no-indent", "-old-syntax"),
    Compile / unmanagedJars := {
      val runtime = file(sys.env.getOrElse("JOERN_RUNTIME", sys.error("Set JOERN_RUNTIME to pinned stock Joern")))
      val directory = runtime / "frontends" / "c2cpg" / "lib"
      require(directory.isDirectory, s"Missing stock frontend libraries: $directory")
      val jars = (directory * "*.jar").get.sorted.filterNot(_.getName.startsWith("io.joern.c2cpg-"))
      require(jars.nonEmpty, "Stock Joern runtime classpath is empty")
      jars.classpath
    },
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    Test / fork := false
  )
