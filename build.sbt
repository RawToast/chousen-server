name := "chousenScript"

version := "1.0"

scalaVersion := "2.12.0"

mainClass in(Compile, run) := Some("chousen.Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %  "cats-core_2.12.0-RC2" % "0.8.0"

libraryDependencies += "com.github.julien-truffaut" % "monocle-core_2.12.0-RC2" % "1.3.1"
