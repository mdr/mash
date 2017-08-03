package com.github.mdr.mash.repl.history

import java.nio.file.Path
import java.time.Instant
import java.util.UUID

import com.github.mdr.mash.runtime.MashValue

case class HistoryEntry(sessionId: UUID,
                        commandNumber: Int,
                        timestamp: Instant,
                        command: String,
                        mish: Boolean,
                        result: MashValue,
                        workingDirectory: Path)
