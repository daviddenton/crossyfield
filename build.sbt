
val orgName = "io.github.daviddenton"

val projectName = "crossyfield"

organization := orgName

name := projectName

description := "Cross-field object extraction for fun and profit"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.4" % "test")

licenses +=("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0"))

pomExtra :=
  <url>http://{projectName}.github.io/</url>
    <scm>
      <url>git@github.com:daviddenton/{projectName}.git</url>
      <connection>scm:git:git@github.com:daviddenton/{projectName}.git</connection>
      <developerConnection>scm:git:git@github.com:daviddenton/{projectName}.git</developerConnection>
    </scm>
    <developers>
      <developer>
        <name>David Denton</name>
        <email>dev@fintrospect.io</email>
        <organization>{projectName}</organization>
        <organizationUrl>http://daviddenton.github.io</organizationUrl>
      </developer>
    </developers>

Seq(bintraySettings: _*)
