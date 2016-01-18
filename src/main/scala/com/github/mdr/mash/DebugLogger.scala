package com.github.mdr.mash

import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.io.FileUtils
import java.io.File

/**
 * A debug logger for debugging internal exceptions
 */
object DebugLogger {

  private val logFile = new File("/tmp/mash-debug.log")

  def logException(e: Throwable) {
    FileUtils.writeStringToFile(logFile, ExceptionUtils.getStackTrace(e), true)
  }

  def logMessage(s: String) {
    FileUtils.writeStringToFile(logFile, s + "\n", true)
  }

}