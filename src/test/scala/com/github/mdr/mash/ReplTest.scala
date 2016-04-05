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
    repl.acceptLine()
    repl.state.globalVariables(ReplState.It) should equal(MashNumber(1))
  }

  "Incremental completion" should "stay incremental as you type characters" in {
    val repl = newRepl
    repl.input("wh")
    repl.complete().text should equal("where")
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    completionState.replacementLocation should equal(Region(0, "where".length))
    val completions = completionState.completions.map(_.text)
    completions should contain("where")
    completions should contain("whereNot")

    repl.input("N")
    val Some(completionState2: IncrementalCompletionState) = repl.state.completionStateOpt
  }

  "History" should "not have a bug if you attempt to go forwards in history past the current" in {
    val repl = newRepl
    repl.input("1").acceptLine()
    repl.input("2").acceptLine()
    repl.text should equal("")
    repl.previousHistory().text should equal("2")
    repl.nextHistory().text should equal("")
    repl.nextHistory().text should equal("")
    repl.previousHistory().text should equal("2")
    repl.previousHistory().text should equal("1")
  }
  
  private def newRepl = new Repl(DummyTerminal, NullPrintStream)

  private implicit class RichRepl(repl: Repl) {

    def input(s: String) = {
      for (c ← s)
        repl.handleAction(InputAction.SelfInsert(c))
        repl
    }
    
    def complete() = { repl.handleAction(InputAction.Complete); repl }

    def previousHistory() = { repl.handleAction(InputAction.PreviousHistory); repl }
    
    def nextHistory() = { repl.handleAction(InputAction.NextHistory); repl }
    
    def acceptLine() = { repl.handleAction(InputAction.AcceptLine); repl }
    
    def text = repl.state.lineBuffer.s
  }

}

object DummyTerminal extends Terminal {

  override def info = TerminalInfo(80, 40)

}

object NullPrintStream extends PrintStream(new OutputStream() {
  override def write(b: Int) {}
})
