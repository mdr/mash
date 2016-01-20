package com.github.mdr.mash

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.terminal.TerminalInfo
import java.io.PrintStream
import java.io.OutputStream
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.utils.Region

class ReplTest extends FlatSpec with Matchers {

  "Repl" should "work" in {
    val repl = newRepl
    repl.input("1")
    repl.handleAction(InputAction.AcceptLine)
    repl.state.globalVariables(ReplState.It) should equal(MashNumber(1))
  }

  "Incremental completion" should "stay incremental as you type characters" in {
    val repl = newRepl
    repl.input("wh")
    repl.handleAction(InputAction.Complete)
    repl.state.lineBuffer.s should equal("where")
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    completionState.replacementLocation should equal(Region(0, "where".length))
    val completions = completionState.completions.map(_.text)
    completions should contain("where")
    completions should contain("whereNot")

    repl.input("N")
    val Some(completionState2: IncrementalCompletionState) = repl.state.completionStateOpt
  }

  private def newRepl = new Repl(DummyTerminal, NullPrintStream)

  private implicit class RichRepl(repl: Repl) {

    def input(s: String) {
      for (c ← s)
        repl.handleAction(InputAction.SelfInsert(c))
    }

  }

}

object DummyTerminal extends Terminal {

  override def info = TerminalInfo(80, 40)

}

object NullPrintStream extends PrintStream(new OutputStream() {
  override def write(b: Int) {}
})
