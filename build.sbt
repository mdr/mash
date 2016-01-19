enablePlugins(DockerPlugin)

name := "mash"

organization := "com.github.mdr"

version := "0.0.2-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % Test,
  "junit" % "junit" % "4.12" % Test,
  "org.pegdown" % "pegdown" % "1.6.0" % Test) // Needed for test reports

libraryDependencies ++= Seq(
  "org.fusesource.jansi" % "jansi" % "1.11",
  "commons-io" % "commons-io" % "2.4",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "com.github.jnr" % "jnr-posix" % "3.0.26",
  "org.apache.ant" % "ant" % "1.9.6",
  "org.ocpsoft.prettytime" % "prettytime" % "4.0.1.Final",
  "jline" % "jline" % "2.13",
  "com.ibm.icu" % "icu4j" % "56.1",
  "com.googlecode.lanterna" % "lanterna" % "3.0.0-beta1",
  "com.google.code.gson" % "gson" % "2.5",
  "com.fatboyindustrial.gson-javatime-serialisers" % "gson-javatime-serialisers" % "1.1.1",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.1.1.201511131810-r",
  "org.slf4j" % "slf4j-nop" % "1.7.13" /* suppress logging from jgit */,
  "com.outr.javasysmon" % "javasysmon_2.10" % "0.3.4")

// == Eclipse integration =====================================================================

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

// == Misc ====================================================================================

mainClass in (Compile, run) := Some("com.github.mdr.mash.Main")

mainClass in assembly := (mainClass in (Compile, run)).value

// javaOptions ++= Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005")

// Fork mode:

// fork in run := true

// connectInput in run := true

// outputStrategy := Some(StdoutOutput)

// == Test ===================================================================================

fork in test := true

testFrameworks := Seq(TestFrameworks.ScalaTest)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDFT", "-h", "target/report") // T => show reminder of failed tests with short stack trace 

// == Docker =================================================================================

docker <<= (docker dependsOn assembly)

imageNames in docker := Seq(ImageName("mattrusselluk/mash:latest"))

dockerfile in docker := {
  val artifact = (outputPath in assembly).value
  val artifactTargetPath = s"/app/${artifact.name}"
  new Dockerfile {
    from("java")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", artifactTargetPath)
  }
}
