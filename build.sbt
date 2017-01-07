name := "chousenScript"

version := "1.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

val SCALA_VERSION = "2.11.8"

scalaVersion := SCALA_VERSION
scalaVersion in ThisBuild := SCALA_VERSION


resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %  "cats-core_2.11" % "0.8.0"

libraryDependencies += "com.github.julien-truffaut" % "monocle-core_2.11" % "1.3.1"
