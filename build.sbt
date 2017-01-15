import NativePackagerKeys._

name := "chousenScript"
herokuAppName in Compile := "immense-bastion-74506"

version := "1.0"

mainClass in(Compile, run) := Some("Main")

//enablePlugins(JettyPlugin)
enablePlugins(JavaServerAppPackaging)
//addCommandAlias("stage", ";clean;compile;package")

val SCALA_VERSION = "2.11.8"


scalaVersion := SCALA_VERSION
scalaVersion in ThisBuild := SCALA_VERSION

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Bintary JCenter" at "http://jcenter.bintray.com"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
//libraryDependencies += "org.typelevel" %  "cats-core_2.11" % "0.8.0"
libraryDependencies += "com.github.julien-truffaut" % "monocle-core_2.11" % "1.3.1"
libraryDependencies += "io.circe" %% "circe-generic" % "0.6.1"

libraryDependencies ++= finch

def finch = Seq(
    "com.github.finagle" %% "finch-core" % "0.11.1",
    "com.github.finagle" %% "finch-circe" % "0.11.1",
    "com.github.finagle" %% "finch-test" % "0.11.1",
    "com.twitter" %% "twitter-server" % "1.25.0")

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
