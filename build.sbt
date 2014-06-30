import play.Project._

name := """ticTacToe"""

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.2.0", 
  "org.webjars" % "bootstrap" % "2.3.1",
  "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test"
)

playScalaSettings
