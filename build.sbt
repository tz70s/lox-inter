val commonSettings = Seq(
  version := "0.1",
  scalaVersion := "2.12.6"
)

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

lazy val `lox-inter` = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies += scalaTest
  )