
enablePlugins(DockerPlugin)

name := "mash"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

// unmanagedJars in Compile += file("lib/javasysmon.jar")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

libraryDependencies += "org.pegdown" % "pegdown" % "1.6.0" % "test" // Needed for test reports

libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.11"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

libraryDependencies += "com.github.jnr" % "jnr-posix" % "3.0.25"

libraryDependencies += "org.apache.ant" % "ant" % "1.9.6"

libraryDependencies += "org.ocpsoft.prettytime" % "prettytime" % "4.0.1.Final"

libraryDependencies += "jline" % "jline" % "2.13"

libraryDependencies += "com.ibm.icu" % "icu4j" % "56.1"

libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "3.0.0-beta1"

libraryDependencies += "com.google.code.gson" % "gson" % "2.5"

libraryDependencies += "com.fatboyindustrial.gson-javatime-serialisers" % "gson-javatime-serialisers" % "1.1.1"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.1.1.201511131810-r"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.13"

// == Eclipse integration =====================================================================

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

resolvers += Resolver.mavenLocal

mainClass in (Compile, run) := Some("com.github.mdr.mash.Main")

mainClass in assembly := Some("com.github.mdr.mash.Main")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDFT") // T => show reminder of failed tests with short stack trace 

// javaOptions ++= Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005")

//fork in run := true

fork in test := true

//connectInput in run := true

//outputStrategy := Some(StdoutOutput)

testFrameworks := Seq(TestFrameworks.ScalaTest)

// Override specs2 options
// & Stop problem with tests executing twice because of "JUnitRunner" annotation:
// (testOptions in Test) := Seq(Tests.Argument(TestFrameworks.JUnit, "--ignore-runners=org.scalatest.junit.JUnitRunner"))

// Generate HTML report
//(testOptions in Test) += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report")

// Docker

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
