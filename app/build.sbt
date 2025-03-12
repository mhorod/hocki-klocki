ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

Compile / mainClass := Some("Main")

lazy val root = (project in file("."))
  .settings(
    name := "wdbn",
    libraryDependencies ++=
      Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
        "com.lihaoyi" %% "upickle" % "4.1.0"
      ),
    assembly / mainClass := Some("Main"), // for sbt-assembly
    assembly / assemblyJarName := "app.jar", // for sbt-assembly
  )

