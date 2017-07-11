package com.github.mdr.mash.repl

import java.io.PrintStream
import java.util.UUID

import com.github.mdr.mash.Config
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.os.{ FileSystem, MockEnvironmentInteractions, MockFileSystem }
import com.github.mdr.mash.repl.LineBufferTestHelper._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ NextColumn, PreviousColumn, UnfocusColumn }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState
import com.github.mdr.mash.repl.completions.{ BrowserCompletionState, IncrementalCompletionState }
import com.github.mdr.mash.repl.history.HistoryImpl
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.terminal.{ Terminal, TerminalInfo }
import org.scalatest._

class ReplTest extends FlatSpec with Matchers {

  import ReplTest._

  "Repl" should "work" in {
    val repl = makeRepl()
    repl.input("1")
    repl.acceptLine()
    repl.state.globalVariables.get(ReplState.It) should equal(Some(MashNumber(1)))
  }

  "Single tab" should "complete a unique completion" in {
    val repl = makeRepl()
    repl.input("whereNo").complete()
    repl.text should equal("whereNot")
    repl.lineBuffer should equal(parseLineBuffer("whereNot▶"))
  }

  "Two tabs" should "enter completions browsing mode" in {
    val repl = makeRepl()
    repl.input("where").complete().complete()
    val Some(_: BrowserCompletionState) = repl.state.completionStateOpt
  }

  "Completion bug after a hyphen" should "not happen" in {
    val repl = makeRepl()
    repl.input("ls -42 # foo").left(8)
    repl.lineBuffer should equal(parseLineBuffer("ls -▶42 # foo"))

    repl.complete()

    repl.lineBuffer should equal(parseLineBuffer("ls -▶42 # foo"))
  }

  "History" should "not have a bug if you attempt to go forwards in history past the current" in {
    val repl = makeRepl()
    repl.input("1").acceptLine()
    repl.input("2").acceptLine()
    repl.text should equal("")
    repl.previousHistory().text should equal("2")
    repl.nextHistory().text should equal("")
    repl.nextHistory().text should equal("")
    repl.previousHistory().text should equal("2")
    repl.previousHistory().text should equal("1")
  }

  "History" should "reset if an old line is edited" in {
    val repl = makeRepl()
    repl.input("command1").acceptLine()
    repl.input("command2").acceptLine()
    repl.input("partial")
    repl.previousHistory().text shouldEqual "command2"
    repl.nextHistory().text shouldEqual "partial"
    repl.previousHistory().backspace().text shouldEqual "command"

    repl.nextHistory()

    repl.text should equal("command")
  }

  "Toggling quotes" should "enclose adjacent string in quotes if unquoted, or remove them if quoted" in {
    val repl = makeRepl()
    repl.input("foo")
    repl.toggleQuote().text shouldEqual """"foo""""
    repl.toggleQuote().text shouldEqual "foo"
  }

  "Delete" should "work at the first character" in {
    val repl = makeRepl()

    repl.input("123").left(3).delete()

    repl.lineBuffer should equal(parseLineBuffer("▶23"))
  }

  "Repl" should "respect bare words setting" in {
    val repl = makeRepl()
    repl.input(s"config.${Config.Language.BareWords} = true").acceptLine()
    repl.input("foo").acceptLine()
    repl.it should equal(MashString("foo"))

    repl.input(s"config.${Config.Language.BareWords} = false").acceptLine()
    repl.input("foo").acceptLine()
    repl.it should equal(MashBoolean.False /* Repl should have emitted an error */)
  }

  "Type inference loop bug" should "not happen" in {
    makeRepl()
      .input("a => a").acceptLine()
      .complete() // previously blew up here
  }

  "Local variables" should "not collide with global" in {
    makeRepl()
      .input("a = 0").acceptLine()
      .input("def setA n = { a = n }").acceptLine()
      .input("setA 42").acceptLine()
      .input("a").acceptLine()
      .it should equal(MashNumber(0))
  }

  "Completing dotfiles" should "not have a bug where the original input is truncated" in {
    val repl = makeRepl(MockFileSystem.of("/.dotfiles/.bashrc"))
    repl.input(""""/.dotfiles/".""").complete()
    repl.text should equal(""""/.dotfiles/."""") // bug was it was "."
  }

  "Multiline editing" should "be supported" in {
    val repl = makeRepl()
    repl
      .input("{").acceptLine()
      .input("  42").acceptLine()
      .input("}").acceptLine()
    repl.it should equal(MashNumber(42))
  }

  "Type inferencer" should "handle previously-defined user-defined nullary functions" in {
    makeRepl()
      .input("foo = { bar: => { baz: 100 } }").acceptLine()
      .input("foo.bar.ba").complete()
      .text should equal("foo.bar.baz")
  }

  "Incremental history search" should "find results matching case-insensitively" in {
    makeRepl()
      .input("foobar = 42").acceptLine()
      .incrementalHistorySearch()
      .input("FOO")
      .text should equal("foobar = 42")
  }

  "Two D table browser" should "allow column selection" in {
    val twoDBrowser =
      makeRepl()
        .input("view.browser [{ a: 1, b: 2 }, { a: 3, b: 4 }]").acceptLine()
        .affirmInTwoDBrowser
    twoDBrowser.currentRow should equal(0)
    twoDBrowser.currentColumnOpt should equal(None)
    twoDBrowser.nextColumn().currentColumnOpt should equal (Some(0))
    twoDBrowser.unfocusColumn().currentColumnOpt should equal (None)
    twoDBrowser.previousColumn().currentColumnOpt should equal (Some(2))
  }

}

object ReplTest {

  def makeRepl(fileSystem: FileSystem = new MockFileSystem) = {
    val history = new HistoryImpl(new InMemoryHistoryStorage())
    val globalVariables = StandardEnvironment.createGlobalVariables()
    new Repl(DummyTerminal(), NullPrintStream, fileSystem, new MockEnvironmentInteractions, history = history,
      sessionId = UUID.randomUUID, globalVariables = globalVariables)
  }

  case class TwoDBrowser(repl: Repl) {

    def currentRow: Int = repl.getTwoDBrowserState.currentRow

    def currentColumnOpt: Option[Int] = repl.getTwoDBrowserState.currentColumnOpt

    def nextColumn() = {
      repl.handleAction(NextColumn)
      this
    }

    def previousColumn() = {
      repl.handleAction(PreviousColumn)
      this
    }

    def unfocusColumn() = {
      repl.handleAction(UnfocusColumn)
      this
    }

  }

  implicit class RichRepl(repl: Repl) {

    import com.github.mdr.mash.repl.NormalActions._

    def input(s: String): Repl = {
      repl.handleAction(SelfInsert(s))
      repl
    }

    def incrementalHistorySearch(): Repl = {
      repl.handleAction(IncrementalHistorySearch)
      repl
    }

    def complete(): Repl = {
      repl.handleAction(Complete)
      repl
    }

    def previousHistory(): Repl = {
      repl.handleAction(PreviousHistory)
      repl
    }

    def nextHistory(): Repl = {
      repl.handleAction(NextHistory)
      repl
    }

    def acceptLine(): Repl = {
      repl.handleAction(AcceptLine)
      repl
    }

    def toggleQuote(): Repl = {
      repl.handleAction(ToggleQuote)
      repl
    }

    def text: String = lineBuffer.text

    def lineBuffer: LineBuffer = repl.state.lineBuffer

    def left(n: Int = 1): Repl = {
      for (i ← 1 to n)
        repl.handleAction(BackwardChar)
      repl
    }

    def delete(): Repl = {
      repl.handleAction(DeleteChar)
      repl
    }

    def backspace(): Repl = {
      repl.handleAction(BackwardDeleteChar)
      repl
    }

    def draw(): Repl = {
      repl.draw()
      repl
    }

    def it: MashValue = {
      repl.state.globalVariables.get(ReplState.It).get
    }

    def incrementalCompletionState = repl.state.completionStateOpt.collect {
      case state: IncrementalCompletionState ⇒ state
    }.getOrElse(throw new AssertionError("Not in incremental completion mode"))

    def affirmInTwoDBrowser = {
      getTwoDBrowserState
      TwoDBrowser(repl)
    }

    def getTwoDBrowserState: TwoDTableBrowserState = {
      val browserStateStack = repl.state.objectBrowserStateStackOpt.getOrElse(throw new AssertionError("Expected 2D browser, but no browser active"))
      browserStateStack.headState match {
        case state: TwoDTableBrowserState ⇒ state
        case state                        ⇒ throw new AssertionError(s"Was not a 2D browser, but a ${state.getClass.getSimpleName}")
      }
    }
  }

}

case class DummyTerminal(width: Int = 80) extends Terminal {

  override def info = TerminalInfo(width, 40)

}

object NullPrintStream extends PrintStream(_ => {
  /* no-op */
})
