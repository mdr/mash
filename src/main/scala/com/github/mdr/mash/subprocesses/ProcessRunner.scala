package com.github.mdr.mash.subprocesses

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.evaluator.MashNumber
import org.fusesource.jansi.Ansi
import org.apache.commons.io.IOUtils
import scala.collection.JavaConverters._

object ProcessRunner {

  private val terminalControl = Singletons.terminalControl
  private val envInteractions: EnvironmentInteractions = LinuxEnvironmentInteractions

  def runProcess(args: Seq[Any], expandTilde: Boolean = false): Int = {
    terminalControl.setEchoEnabled(true)
    var stringArgs = args.map(ToStringifier.stringify)
    if (expandTilde) {
      val tildeExpander = new TildeExpander(envInteractions)
      stringArgs = stringArgs.map(tildeExpander.expand)
    }
    try {
      terminalControl.configureTerminalForExternalProcess()
      val process =
        new ProcessBuilder(stringArgs: _*)
          .redirectInput(ProcessBuilder.Redirect.INHERIT)
          .redirectOutput(ProcessBuilder.Redirect.INHERIT)
          .redirectError(ProcessBuilder.Redirect.INHERIT).start()
      val statusCode = process.waitFor()

      // Clear out any partial output
      System.out.write(("\r" + Ansi.ansi().eraseLine()).getBytes)
      System.out.flush()

      statusCode
    } finally
      terminalControl.restore()
  }

  def runAndCaptureProcess(args: Seq[Any], expandTilde: Boolean = false): ProcessResult = {
    terminalControl.setEchoEnabled(true)
    var stringArgs = args.map(ToStringifier.stringify)
    if (expandTilde) {
      val tildeExpander = new TildeExpander(envInteractions)
      stringArgs = stringArgs.map(tildeExpander.expand)
    }
    try {
      terminalControl.configureTerminalForExternalProcess()
      val process =
        new ProcessBuilder(stringArgs: _*)
          .redirectInput(ProcessBuilder.Redirect.INHERIT)
          .redirectOutput(ProcessBuilder.Redirect.PIPE)
          .redirectError(ProcessBuilder.Redirect.INHERIT).start()
      val stdout = IOUtils.toString(process.getInputStream)
      val statusCode = process.waitFor()

      // Clear out any partial output
      System.out.write(("\r" + Ansi.ansi().eraseLine()).getBytes)
      System.out.flush()
      ProcessResult(statusCode, stdout)
    } finally
      terminalControl.restore()
  }
}

case class ProcessResult(exitStatus: Int, stdout: String)