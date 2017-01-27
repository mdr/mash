package com.github.mdr.mash.repl.history

import java.nio.file.{ Path, Paths }
import java.time.Instant
import java.util.UUID

import com.github.mdr.mash.runtime.MashValue

case class HistoryEntry(sessionId: UUID,
                        commandNumber: Int,
                        timestamp: Instant,
                        command: String,
                        mish: Boolean,
                        result: MashValue,
                        workingDirectory: String) {

  def sessionIdOpt: Option[UUID] = Option(sessionId)

  def resultOpt: Option[MashValue] = Option(result)

  def workingDirectoryOpt: Option[Path] = Option(workingDirectory).map(s ⇒ Paths.get(s))

}
