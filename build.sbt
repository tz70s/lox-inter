val commonSettings = Seq(
  version := "0.1",
  scalaVersion := "2.12.6"
)

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"
val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

val libraries = Seq(
  scalaTest, logback, scalaLogging
)

lazy val `lox-inter` = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies ++= libraries
  )