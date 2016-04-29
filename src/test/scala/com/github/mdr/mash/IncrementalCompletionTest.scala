package com.github.mdr.mash

import org.scalatest._

import com.github.mdr.mash.repl.IncrementalCompletionState
import com.github.mdr.mash.repl.Repl
import com.github.mdr.mash.utils.Region

class IncrementalCompletionTest extends FlatSpec with Matchers {

  import ReplTest._

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

  it should "leave incremental completion mode if you type an exact match" in {
    val repl = newRepl
    repl.input("where").complete().input("Not")
    repl.state.completionStateOpt should equal(None)
  }

  it should "leave incremental completion mode if no longer have any matches" in {
    val repl = newRepl
    repl.input("where").complete().input("a")
    repl.state.completionStateOpt should equal(None)
  }

  it should "should remove added characters by pressing backspace" in {
    val repl = newRepl
    repl.input("where").complete().input("N").backspace()

    repl.text should equal("where")
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    val completions = completionState.completions.map(_.text)
    completions should contain("where")
    completions should contain("whereNot")
  }

  it should "should allow further tab completions" in {
    val repl = newRepl
    repl.input("where").complete().input("N").complete()

    repl.text should equal("whereNot")
    repl.state.completionStateOpt should equal(None)
  }

  it should "leave incremental completion mode if backspace past the first completion" in {
    val repl = newRepl
    repl.input("where").complete().input("N").backspace().backspace()
    repl.state.completionStateOpt should equal(None)
  }

  private def newRepl = new Repl(DummyTerminal(), NullPrintStream)

}
