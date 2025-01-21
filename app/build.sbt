ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

Compile / mainClass := Some("Main")

lazy val root = (project in file("."))
  .settings(
    name := "wdbn",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    assembly / mainClass := Some("Main"), // for sbt-assembly
  )

