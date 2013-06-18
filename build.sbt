name := "updatable"

organization := "org.hablapps"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-feature")

version := "1.0"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % "test",
  "org.scala-lang" % "scala-actors" % "2.10.2",
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "org.scala-lang" % "scala-compiler" % "2.10.2",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test")
