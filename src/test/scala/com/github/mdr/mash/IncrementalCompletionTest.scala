package com.github.mdr.mash

import org.scalatest._
import com.github.mdr.mash.repl.IncrementalCompletionState
import com.github.mdr.mash.repl.Repl
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.MockFileSystem
import com.github.mdr.mash.LineBufferTestHelper._
import com.github.mdr.mash.os.MockFileObject._

class IncrementalCompletionTest extends FlatSpec with Matchers {

  import ReplTest._

  "Incremental completion" should "stay incremental as you type characters" in {
    val repl = newRepl
    repl.input("wh")
    repl.complete().text should equal("where")
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    completionState.replacementLocation should equal(Region(0, "where".length))
    val completions = completionState.completions.map(_.displayText)
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
    val completions = completionState.completions.map(_.displayText)
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

  it should "partially complete a common prefix, handling any required escaping" in {
    val repl = ReplTest.makeRepl(
      new MockFileSystem(Directory(
        "foo$bar" -> File(),
        "foo$baz" -> File())))
    repl.input("foo")

    repl.complete()

    repl.lineBuffer should equal(parseLineBuffer(""""foo\$ba"◀"""))
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    val completions = completionState.completions.map(_.displayText)
    completions should equal(Seq("foo$bar", "foo$baz"))
  }

  private def newRepl = ReplTest.makeRepl()

}
