name := "chousenScript"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile, run) := Some("chousen.Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

    