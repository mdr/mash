package com.github.mdr.mash

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.UUID

import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.repl.ScriptExecutor
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.Terminal
import org.apache.commons.io.FileUtils

object InitScriptRunner {

  val InitFile = "init.mash"
  val InitPath = Mash.MashDir.resolve(InitFile)

}

class InitScriptRunner(terminal: Terminal,
                       output: PrintStream,
                       sessionId: UUID,
                       globalVariables: MashObject) {

  import InitScriptRunner._

  private val debugLogger = new DebugLogger(sessionId.toString)

  private def getInitScript: Option[CompilationUnit] = {
    Mash.ensureMashDirExists()
    if (Files.exists(InitPath))
      try {
        val s = FileUtils.readFileToString(InitPath.toFile, StandardCharsets.UTF_8)
        Some(CompilationUnit(s, name = InitFile, mish = false))
      } catch {
        case e: Exception ⇒
          output.println("Error reading " + InitPath)
          e.printStackTrace(output)
          debugLogger.logException(e)
          None
      }
    else
      None
  }

  def processInitFile() =
    for (initScript ← getInitScript) {
      val scriptExecutor = new ScriptExecutor(output, terminal, sessionId, globalVariables)
      scriptExecutor.runUnit(initScript)
    }

}
