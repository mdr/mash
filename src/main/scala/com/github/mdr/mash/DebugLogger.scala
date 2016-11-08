package com.github.mdr.mash

import java.io.File
import java.nio.charset.StandardCharsets
import java.util.UUID

import org.apache.commons.io.FileUtils
import org.apache.commons.lang3.exception.ExceptionUtils

/**
 * A debug logger for debugging internal exceptions
 */
class DebugLogger(sessionId: UUID) {

  private val logFile = new File(s"/tmp/mash-debug-$sessionId.log")

  def logException(e: Throwable) {
    val stackTrace = ExceptionUtils.getStackTrace(e)
    try
      FileUtils.writeStringToFile(logFile, stackTrace, StandardCharsets.UTF_8, true)
    catch {
      case _: Exception ⇒
    }
  }

  def logMessage(s: String) {
    try
      FileUtils.writeStringToFile(logFile, s + "\n", StandardCharsets.UTF_8, true)
    catch {
      case _: Exception ⇒
    }
  }

}