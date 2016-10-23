name := "chousenScript"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in(Compile, run) := Some("chousen.Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "0.7.0"

libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "1.3.1"



    