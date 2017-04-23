import NativePackagerKeys._

name := "chousen-server"
herokuAppName in Compile := "immense-bastion-74506"

version := "1.0"

mainClass in(Compile, run) := Some("chousen.ChousenServer")

enablePlugins(JavaAppPackaging)

val SCALA_VERSION = "2.12.2"
val FINCH_VERSION = "0.14.0"

scalaVersion := SCALA_VERSION
scalaVersion in ThisBuild := SCALA_VERSION

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Bintary JCenter" at "http://jcenter.bintray.com"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.typelevel" %%  "cats" % "0.9.0"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "1.4.0"
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-macro" % "1.4.0"
libraryDependencies += "io.circe" %% "circe-generic" % "0.7.1"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies ++= finch

def finch = Seq(
    "com.github.finagle" %% "finch-core" % FINCH_VERSION,
    "com.github.finagle" %% "finch-circe" % FINCH_VERSION,
    "com.github.finagle" %% "finch-test" % FINCH_VERSION,
    "com.twitter" %% "twitter-server" % "1.28.0")

// Code coverage
addCommandAlias("validate", ";coverage;test;coverageReport")

coverageMinimum := 55 // Continually increase
coverageFailOnMinimum := true

// Compiler options
val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xlint:_",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)
val additionalOptions = Seq(
  "-Ywarn-dead-code",
  "-Ywarn-value-discard"
)

scalacOptions in(Compile, doc) ++= compilerOptions ++ additionalOptions
scalacOptions in Test ++= compilerOptions
