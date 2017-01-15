name := "chousenScript"

version := "1.0"

mainClass in(Compile, run) := Some("Main")

enablePlugins(JettyPlugin)

addCommandAlias("stage", ";clean;compile;package")

val SCALA_VERSION = "2.11.8"

scalaVersion := SCALA_VERSION
scalaVersion in ThisBuild := SCALA_VERSION

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Bintary JCenter" at "http://jcenter.bintray.com"
)

libraryDependencies += "com.github.finagle" %% "finch-core" % "0.11.1"
libraryDependencies += "com.github.finagle" %% "finch-circe" % "0.11.1"
libraryDependencies += "com.github.finagle" %% "finch-test" % "0.11.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %  "cats-core_2.11" % "0.8.0"
libraryDependencies += "com.github.julien-truffaut" % "monocle-core_2.11" % "1.3.1"
libraryDependencies += "com.twitter" %% "twitter-server" % "1.25.0"

val circeVersion = "0.6.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-generic"
).map(_ % circeVersion)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xlint:_",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)
