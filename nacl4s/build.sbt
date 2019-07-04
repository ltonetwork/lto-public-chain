name := "nacl4s"

organization := "com.github.emstlk"

version := "1.1.0-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
//  "-Xlint",
  "-Xfuture"
)

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.0.4"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

coverageExcludedPackages := "com\\.emstlk\\.nacl4s\\.crypto\\.sign\\.Const;com\\.emstlk\\.nacl4s\\.benchmark"

licenses := Seq("MIT License" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/emstlk/nacl4s"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/emstlk/nacl4s"),
    "scm:git:git@github.com:emstlk/nacl4s.git"
  ))

enablePlugins(JmhPlugin)
