ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = project in file(".") settings (
  idePackagePrefix := Some("otfs"),
  name := "OTFS",
)

libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.18"

libraryDependencies += "org.scalanlp" % "breeze_3" % "2.1.0"

libraryDependencies += "org.scodec" % "scodec-core_3" % "2.3.0"
libraryDependencies += "org.scodec" % "scodec-bits_3" % "1.2.0"