package com.github.mdr.mash.subprocesses

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import org.apache.commons.io.IOUtils
import org.fusesource.jansi.Ansi
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.runtime.MashValue
import java.nio.file.Path

object ProcessRunner {

  private val terminalControl = Singletons.terminalControl
  private val envInteractions: EnvironmentInteractions = LinuxEnvironmentInteractions
  private val output: PrintStream = System.out

  def runProcess(args: Seq[MashValue],
                 captureProcess: Boolean = false,
                 stdinRedirectOpt: Option[Path] = None,
                 stdoutRedirectOpt: Option[Path] = None): ProcessResult = {
    terminalControl.setEchoEnabled(true)
    val stringArgs = args.map(ToStringifier.stringify)
    val outputRedirect = stdoutRedirectOpt match {
      case Some(path)          ⇒ ProcessBuilder.Redirect.to(path.toFile)
      case _ if captureProcess ⇒ ProcessBuilder.Redirect.PIPE
      case _                   ⇒ ProcessBuilder.Redirect.INHERIT
    }
    val inputRedirect = stdinRedirectOpt match {
      case Some(path) ⇒ ProcessBuilder.Redirect.from(path.toFile)
      case _          ⇒ ProcessBuilder.Redirect.INHERIT
    }
    terminalControl.externalProcess {
      val builder = new ProcessBuilder(stringArgs: _*)
      builder.redirectInput(inputRedirect)
      builder.redirectOutput(outputRedirect)
      builder.redirectError(ProcessBuilder.Redirect.INHERIT)
      setEnvironment(builder.environment())
      val start = Instant.now
      val process = builder.start()

      val stdout =
        if (captureProcess)
          IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
        else
          ""
      val statusCode = process.waitFor()
      val stop = Instant.now
      // Clear out any partial output
      output.write(("\r" + Ansi.ansi().eraseLine()).getBytes)
      output.flush()

      ProcessResult(statusCode, stdout, start, stop)
    }
  }

  private def setEnvironment(env: java.util.Map[String, String]) = {
    env.clear()
    for ((k, v) ← Singletons.environment.fields)
      env.put(k, ToStringifier.stringify(v))
  }

}

case class ProcessResult(exitStatus: Int, stdout: String, start: Instant, stop: Instant)