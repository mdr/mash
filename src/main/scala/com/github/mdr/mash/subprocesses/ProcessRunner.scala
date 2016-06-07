package com.github.mdr.mash.subprocesses

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.runtime.MashNumber
import org.fusesource.jansi.Ansi
import org.apache.commons.io.IOUtils
import scala.collection.JavaConverters._
import java.nio.charset.StandardCharsets
import java.io.PrintStream
import com.github.mdr.mash.runtime.MashValue
import java.time.Instant
import java.time.Duration

object ProcessRunner {

  private val terminalControl = Singletons.terminalControl
  private val envInteractions: EnvironmentInteractions = LinuxEnvironmentInteractions
  private val output: PrintStream = System.out

  def runProcess(args: Seq[MashValue], expandTilde: Boolean = false, captureProcess: Boolean = false): ProcessResult = {
    terminalControl.setEchoEnabled(true)
    var stringArgs = args.map(ToStringifier.stringify)
    if (expandTilde) {
      val tildeExpander = new TildeExpander(envInteractions)
      stringArgs = stringArgs.map(tildeExpander.expand)
    }

    val outputRedirect = if (captureProcess) ProcessBuilder.Redirect.PIPE else ProcessBuilder.Redirect.INHERIT
    try {
      terminalControl.configureTerminalForExternalProcess()
      val builder = new ProcessBuilder(stringArgs: _*)
        .redirectInput(ProcessBuilder.Redirect.INHERIT)
        .redirectOutput(outputRedirect)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
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
    } finally
      terminalControl.restore()
  }

}

case class ProcessResult(exitStatus: Int, stdout: String, start: Instant, stop: Instant)