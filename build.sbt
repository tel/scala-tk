name := "oi"
normalizedName := "oi"

organization := "com.jspha"
version := "0.1.0-SNAPSHOT"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-deprecation", "-feature", "-Xfatal-warnings")

homepage :=
  Some(url("http://github.com/tel/scala-oi"))

licenses := Seq(
  "BSD3" -> url("https://opensource.org/licenses/BSD-3-Clause")
)

scmInfo := Some(ScmInfo(
  url("https://github.com/tel/scala-oi"),
  "scm:git:git@github.com:tel/scala-oi.git",
  Some("scm:git:git@github.com:tel/scala-oi.git")))

pomExtra in Global := {
  <developers>
    <developer>
      <id>tel</id>
      <name>Joseph Tel Abrahamson</name>
      <url>jspha.com</url>
    </developer>
  </developers>
}