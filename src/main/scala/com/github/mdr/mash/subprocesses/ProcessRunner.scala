package com.github.mdr.mash.subprocesses

import java.io.{ IOException, PrintStream }
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.time.Instant

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.{ EvaluatorException, ToStringifier }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.terminal.ansi.EscapeSequence
import org.apache.commons.io.IOUtils

object ProcessRunner {

  private val terminalControl = Singletons.terminalControl
  private val output: PrintStream = System.out

  def runProcess(args: Seq[MashValue],
                 captureProcess: Boolean = false,
                 stdinRedirectOpt: Option[Path] = None,
                 stdinImmediateOpt: Option[String] = None,
                 stdoutRedirectOpt: Option[Path] = None): ProcessResult = {
    val stringArgs = args.map(ToStringifier.stringify)
    val outputRedirect = getOutputRedirect(captureProcess, stdoutRedirectOpt)
    val inputRedirect = getInputRedirect(stdinRedirectOpt, stdinImmediateOpt)
    terminalControl.externalProcess {
      val builder = new ProcessBuilder(stringArgs: _*)
        .redirectInput(inputRedirect)
        .redirectOutput(outputRedirect)
        .redirectError(ProcessBuilder.Redirect.INHERIT)
      setEnvironment(builder.environment())
      val start = Instant.now
      val process =
        try
          builder.start()
        catch {
          case e: IOException ⇒ throw EvaluatorException(e.getMessage)
        }

      for (stdinImmediate ← stdinImmediateOpt)
        writeStdinImmediate(process, stdinImmediate)

      val stdout = if (captureProcess) IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8) else ""

      val statusCode = process.waitFor()
      val stop = Instant.now
      clearPartialOutput()

      ProcessResult(statusCode, stdout, start, stop)
    }
  }

  // Clear out any partial output
  def clearPartialOutput() {
    output.write(("\r" + EscapeSequence.EraseLineFromCursor).getBytes)
    output.flush()
  }

  def writeStdinImmediate(process: Process, stdinImmediate: String): Unit = {
    IOUtils.write(stdinImmediate, process.getOutputStream, StandardCharsets.UTF_8)
    process.getOutputStream.close()
  }

  def getOutputRedirect(captureProcess: Boolean, stdoutRedirectOpt: Option[Path]): Redirect = {
    stdoutRedirectOpt match {
      case Some(path)          ⇒ ProcessBuilder.Redirect.to(path.toFile)
      case _ if captureProcess ⇒ ProcessBuilder.Redirect.PIPE
      case _                   ⇒ ProcessBuilder.Redirect.INHERIT
    }
  }

  def getInputRedirect(stdinRedirectOpt: Option[Path], stdinImmediateOpt: Option[String]): Redirect = {
    stdinRedirectOpt match {
      case Some(path) ⇒ ProcessBuilder.Redirect.from(path.toFile)
      case _          ⇒
        stdinImmediateOpt match {
          case Some(_) ⇒ ProcessBuilder.Redirect.PIPE
          case _       ⇒ ProcessBuilder.Redirect.INHERIT
        }
    }
  }

  private def setEnvironment(env: java.util.Map[String, String]) = {
    env.clear()
    for ((k, v) ← Singletons.environment.immutableFields)
      env.put(ToStringifier.stringify(k), ToStringifier.stringify(v))
  }

}

case class ProcessResult(exitStatus: Int, stdout: String, start: Instant, stop: Instant)