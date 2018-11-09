name := "csv"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

// POM settings for Sonatype
organization := "com.github.doohochang"

homepage := Some(url("https://github.com/doohochang/csv"))

scmInfo := Some(ScmInfo(
  url("https://github.com/doohochang/csv"),
  "git@github.com:doohochang/csv.git"
))

developers := List(Developer("doohochang",
  "Dooho Chang",
  "doohochang@gmail.com",
  url("https://github.com/doohochang")))

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

// Add Sonatype repository settings
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
