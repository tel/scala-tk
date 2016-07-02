name := "tk"
normalizedName := "tk"

organization := "com.jspha"
version := "0.1.0-SNAPSHOT"
scalaVersion := "2.11.8"
scalacOptions ++= Seq("-deprecation", "-feature", "-Xfatal-warnings")

homepage :=
  Some(url("http://github.com/tel/scala-tk"))

licenses := Seq(
  "BSD3" -> url("https://opensource.org/licenses/BSD-3-Clause")
)

libraryDependencies += "org.typelevel" %% "cats-core" % "0.6.0"

scmInfo := Some(ScmInfo(
  url("https://github.com/tel/scala-tk"),
  "scm:git:git@github.com:tel/scala-tk.git",
  Some("scm:git:git@github.com:tel/scala-tk.git")))

pomExtra in Global := {
  <developers>
    <developer>
      <id>tel</id>
      <name>Joseph Tel Abrahamson</name>
      <url>jspha.com</url>
    </developer>
  </developers>
}