name := "mash"

organization := "com.github.mdr"

version := "0.0.7-SNAPSHOT"

maintainer := "Matt Russell <MattRussellUK@gmail.com>"

packageSummary := "Mash Object Shell"

packageDescription := "An object shell for Unix"

scalaVersion := "2.12.1"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "junit" % "junit" % "4.12" % Test,
  "com.lihaoyi" % "ammonite" % "0.8.1" % Test cross CrossVersion.full,
  "org.pegdown" % "pegdown" % "1.6.0" % Test) // Needed for test reports

libraryDependencies ++= Seq(
  "org.fusesource.jansi" % "jansi" % "1.12",
  "commons-io" % "commons-io" % "2.5",
  "org.apache.commons" % "commons-lang3" % "3.5",
  "com.github.jnr" % "jnr-posix" % "3.0.33",
  "org.apache.ant" % "ant" % "1.10.0",
  "org.ocpsoft.prettytime" % "prettytime" % "4.0.1.Final",
  "jline" % "jline" % "2.14.3",
  "com.ibm.icu" % "icu4j" % "58.2",
  "com.googlecode.lanterna" % "lanterna" % "3.0.0-beta3",
  "com.google.code.gson" % "gson" % "2.8.0",
  "com.fatboyindustrial.gson-javatime-serialisers" % "gson-javatime-serialisers" % "1.1.1",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.6.0.201612231935-r",
  "org.slf4j" % "slf4j-nop" % "1.7.22" /* suppress logging from jgit */,
  "com.outr.javasysmon" % "javasysmon_2.10" % "0.3.4",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.apache.httpcomponents" % "httpclient" % "4.5.2",
  "com.joestelmach" % "natty" % "0.12",
  "net.grey-panther" % "natural-comparator" % "1.1",
  "org.json" % "json" % "20160810")

// == Eclipse integration =====================================================================

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

// == Misc ====================================================================================

mainClass in (Compile, run) := Some("com.github.mdr.mash.Main")

mainClass in assembly := (mainClass in (Compile, run)).value

assemblyJarName in assembly := s"mash-${version.value}.jar"

// javaOptions ++= Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005")

// Fork mode:

// fork in run := true

// connectInput in run := true

// outputStrategy := Some(StdoutOutput)

enablePlugins(BuildInfoPlugin)

buildInfoKeys := Seq[BuildInfoKey](version, "commit" -> { git.gitHeadCommit.value })

buildInfoPackage := "com.github.mdr.mash.build"

// == Test ===================================================================================

fork in test := true

testFrameworks := Seq(TestFrameworks.ScalaTest)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDFT", "-h", "target/report") // T => show reminder of failed tests with short stack trace

initialCommands in (Test, console) := "ammonite.Main().run()"

// Packaging

enablePlugins(JavaAppPackaging)

debianPackageDependencies in Debian ++= Seq("default-jre")
