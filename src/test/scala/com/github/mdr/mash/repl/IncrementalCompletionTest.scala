package com.github.mdr.mash.repl

import com.github.mdr.mash.os.MockFileObject._
import com.github.mdr.mash.os.MockFileSystem
import com.github.mdr.mash.repl.LineBufferTestHelper._
import com.github.mdr.mash.utils.Region
import org.scalatest._

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
    val completionState = repl.incrementalCompletionState
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

  it should "partially complete a common fragment" in {
    val repl = ReplTest.makeRepl(
      new MockFileSystem(Directory(
        "---foobar---" -> File(),
        "--goobab--" -> File())))
    repl.input("ob")

    repl.complete()

    repl.lineBuffer should equal(parseLineBuffer(""""ooba"◀"""))
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    val completions = completionState.completions.map(_.displayText)
    completions should equal(Seq("---foobar---", "--goobab--"))
    completionState.getCommonDisplayFragment.text should equal("ooba")
  }

  it should "handle escaped characters in a common fragment" in {
    val repl = ReplTest.makeRepl(new MockFileSystem(Directory(
      "---foob$ar---" -> File(),
      "--goob$ab--" -> File())))
    repl.input("ob")

    repl.complete()

    repl.lineBuffer should equal(parseLineBuffer(""""oob\$a"◀"""))
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    val completions = completionState.completions.map(_.displayText)
    completions should equal(Seq("---foob$ar---", "--goob$ab--"))
    completionState.getCommonDisplayFragment.text should equal("oob$a")
  }

  it should "not have a bug after completing a path" in {
    val repl = makeRepl(new MockFileSystem(Directory(
      "etc" -> Directory(
        "foo.conf" -> File(),
        "bar.conf" -> File()))))

    repl.input("/etc/").complete()

    repl.lineBuffer should equal(parseLineBuffer(""""/etc/▶""""))
  }

  it should "substring complete with path prefixes" in {
    val repl = makeRepl(new MockFileSystem(Directory(
      "etc" -> Directory(
        "foobar" -> File(),
        "gooban" -> File()))))

    repl.input("/etc/ob").complete()

    repl.lineBuffer should equal(parseLineBuffer(""""/etc/ooba▶""""))
    val Some(completionState: IncrementalCompletionState) = repl.state.completionStateOpt
    completionState.getCommonDisplayFragment.prefix should equal("/etc/")
    completionState.getCommonDisplayFragment.text should equal("ooba")
    val completions = completionState.completions.map(_.displayText)
    completions should equal(Seq("/etc/foobar", "/etc/gooban"))
  }

  private def newRepl = ReplTest.makeRepl()

}
