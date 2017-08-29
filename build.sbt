import NativePackagerKeys._
import sbt.Keys.libraryDependencies

name := "chousen-server"

version := "0.4.6"

//lazy val root = (project in file(".")).
//  enablePlugins(BuildInfoPlugin).
//  settings(
//    buildInfoKeys := Seq[BuildInfoKey](
//      name, version, scalaVersion, sbtVersion,
//      "buildTimestamp" -> new java.util.Date(System.currentTimeMillis()),
//      "gitHash" -> new java.lang.Object(){
//        override def toString(): String = {
//          try {
//            val extracted = new java.io.InputStreamReader(
//              java.lang.Runtime.getRuntime().exec("git rev-parse HEAD").getInputStream())
//            (new java.io.BufferedReader(extracted)).readLine()
//          } catch {      case t: Throwable => "get git hash failed"}
//        }}.toString()
//    ),
//    buildInfoPackage := "chousen"
//  )


mainClass in(Compile, run) := Some("chousen.Http4sServer")

enablePlugins(JavaAppPackaging, SbtTwirl)


val SCALA_VERSION = "2.12.3"
val FINCH_VERSION = "0.14.0"
val HTTP4S_VERSION = "0.17.0-M2"
val CIRCE_VERSION = "0.8.0"

scalaVersion := SCALA_VERSION
scalaVersion in ThisBuild := SCALA_VERSION

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Bintary JCenter" at "http://jcenter.bintray.com"
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.1"
libraryDependencies += "com.google.api-client" % "google-api-client" % "1.22.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"
// libraryDependencies ++= finch
libraryDependencies ++= http4s
libraryDependencies ++= circe
libraryDependencies ++= monocle

// mango
libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "2.1.0"



def finch = Seq(
    "com.github.finagle" %% "finch-core" % FINCH_VERSION,
    "com.github.finagle" %% "finch-circe" % FINCH_VERSION,
    "com.github.finagle" %% "finch-test" % FINCH_VERSION,
    "com.twitter" %% "twitter-server" % "1.28.0")


def http4s = Seq(
  "org.http4s" %% "http4s-dsl" % HTTP4S_VERSION,
  "org.http4s" %% "http4s-blaze-server" % HTTP4S_VERSION,
  "org.http4s" %% "http4s-blaze-client" % HTTP4S_VERSION,
  "org.http4s" %% "http4s-circe" % HTTP4S_VERSION,
  "org.http4s" %% "http4s-twirl" % HTTP4S_VERSION
)


def circe = Seq(
  "io.circe" %% "circe-generic" % CIRCE_VERSION,
  "io.circe" %% "circe-generic-extras" % CIRCE_VERSION,
  "io.circe" %% "circe-literal" % CIRCE_VERSION,
  "io.circe" %% "circe-optics" % CIRCE_VERSION,
  "io.circe" %% "circe-parser" % CIRCE_VERSION
)


def monocle = Seq(
  "com.github.julien-truffaut" %% "monocle-core" % "1.4.0",
  "com.github.julien-truffaut" %%  "monocle-macro" % "1.4.0"
)

// Code coverage
addCommandAlias("validate", ";coverage;test;coverageReport")

coverageMinimum := 70
coverageFailOnMinimum := true

TwirlKeys.templateImports := Seq()

// Compiler options
val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xlint:_",
   //"-Xfatal-warnings",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  //"-Ywarn-unused-import",
  "-Ywarn-value-discard"
)
val additionalOptions = Seq(
  //"-Ywarn-unused",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard"
)

scalacOptions in Compile ++= compilerOptions ++ additionalOptions
scalacOptions in Test ++= compilerOptions
