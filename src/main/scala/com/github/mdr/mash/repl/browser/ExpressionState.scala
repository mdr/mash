package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.repl.completions.CompletionState

case class ExpressionState(lineBuffer: LineBuffer,
                           completionStateOpt: Option[CompletionState] = None)
