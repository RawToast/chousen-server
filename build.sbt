import NativePackagerKeys._

name := "chousen-server"
herokuAppName in Compile := "immense-bastion-74506"

version := "1.0"

mainClass in(Compile, run) := Some("Main")

enablePlugins(JavaAppPackaging)

val SCALA_VERSION = "2.11.8"
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)


scalaVersion := SCALA_VERSION
scalaVersion in ThisBuild := SCALA_VERSION

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Bintary JCenter" at "http://jcenter.bintray.com"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %%  "cats-core" % "0.9.0"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "1.4.0"
libraryDependencies +="com.github.julien-truffaut" %%  "monocle-macro" % "1.4.0"

libraryDependencies += "io.circe" %% "circe-generic" % "0.7.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies ++= finch

def finch = Seq(
    "com.github.finagle" %% "finch-core" % "0.12.0",
    "com.github.finagle" %% "finch-circe" % "0.12.0",
    "com.github.finagle" %% "finch-test" % "0.12.0",
    "com.twitter" %% "twitter-server" % "1.26.0")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  //"-Xlint:_",
  //"-Xfatal-warnings",
  "-Xfuture",
  //"-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  //"-Ywarn-unused",
  //"-Ywarn-unused-import",
  "-Ywarn-value-discard"
)
