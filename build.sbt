name := "updatable"

organization := "org.hablapps"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-feature")

version := "1.0"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.8.1" % "test",
  "org.scala-lang" % "scala-actors" % "2.10.0",
  "org.scala-lang" % "scala-reflect" % "2.10.0",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)
