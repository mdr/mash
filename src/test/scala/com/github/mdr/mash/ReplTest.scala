package com.github.mdr.mash

import java.io.OutputStream
import java.io.PrintStream
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.repl.Repl
import com.github.mdr.mash.repl.IncrementalCompletionState

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

  "Incremental completion" should "leave incremental completion mode if you type an exact match" in {
    val repl = newRepl
    repl.input("where").complete().input("Not")
    repl.state.completionStateOpt should equal(None)
  }

  "Incremental completion" should "leave incremental completion mode if no longer have any matches" in {
    val repl = newRepl
    repl.input("where").complete().input("a")
    repl.state.completionStateOpt should equal(None)
  }

  "Incremental completion" should "should return to prior states after pressing backspace" in {
    val repl = newRepl
    repl.input("where").complete().input("N").backspace()

    repl.text should equal("where")
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    val completions = completionState.completions.map(_.text)
    completions should contain("where")
    completions should contain("whereNot")
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

  "Toggling quotes" should "enclose adjacent string in quotes if unquoted, or remove them if quoted" in {
    val repl = newRepl
    repl.input("foo")
    repl.toggleQuote().text should equal(""""foo"""")
    repl.toggleQuote().text should equal("foo")
  }

  "Delete" should "work at the first character" in {
    val repl = newRepl

    repl.input("123").left(3).delete()

    repl.text should equal("23")
    repl.cursorPos should equal(0)
  }

  private def newRepl = new Repl(DummyTerminal, NullPrintStream)

  private implicit class RichRepl(repl: Repl) {

    def input(s: String): Repl = {
      repl.handleAction(InputAction.SelfInsert(s))
      repl
    }

    def complete(): Repl = { repl.handleAction(InputAction.Complete); repl }

    def previousHistory(): Repl = { repl.handleAction(InputAction.PreviousHistory); repl }

    def nextHistory(): Repl = { repl.handleAction(InputAction.NextHistory); repl }

    def acceptLine(): Repl = { repl.handleAction(InputAction.AcceptLine); repl }

    def toggleQuote(): Repl = { repl.handleAction(InputAction.ToggleQuote); repl }

    def text: String = repl.state.lineBuffer.text

    def cursorPos: Int = repl.state.lineBuffer.cursorPos

    def left(n: Int = 1): Repl = {
      for (i ← 1 to n)
        repl.handleAction(InputAction.BackwardChar)
      repl
    }

    def delete(): Repl = { repl.handleAction(InputAction.DeleteChar); repl }

    def backspace(): Repl = { repl.handleAction(InputAction.BackwardDeleteChar); repl }

  }

}

object DummyTerminal extends Terminal {

  override def info = TerminalInfo(80, 40)

}

object NullPrintStream extends PrintStream(new OutputStream() {
  override def write(b: Int) { /* no-op */ }
})
