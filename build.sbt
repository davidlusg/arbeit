name := "jpm-davidlu"

version := "0.1"

scalaVersion := "2.12.7"

lazy val root = (project in file(".")).
  settings(
    name := "jpm-davidlu",
    version := "1.0",
    scalaVersion := "2.12.7",
    mainClass in Compile := Some("SpellChecker")
  )