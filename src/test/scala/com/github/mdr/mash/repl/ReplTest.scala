package com.github.mdr.mash.repl

import java.io.OutputStream
import java.io.PrintStream
import org.scalatest._
import com.github.mdr.mash.Config
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.MockEnvironmentInteractions
import com.github.mdr.mash.os.MockFileSystem
import com.github.mdr.mash.repl.LineBufferTestHelper._
import com.github.mdr.mash.repl.NormalActions.SelfInsert
import com.github.mdr.mash.repl.history.HistoryImpl
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue

class ReplTest extends FlatSpec with Matchers {

  import ReplTest._

  "Repl" should "work" in {
    val repl = newRepl
    repl.input("1")
    repl.acceptLine()
    repl.state.globalVariables(ReplState.It) should equal(MashNumber(1))
  }

  "Single tab" should "complete a unique completion" in {
    val repl = newRepl
    repl.input("whereNo").complete()
    repl.text should equal("whereNot")
    repl.lineBuffer should equal(parseLineBuffer("whereNot▶"))
  }

  "Two tabs" should "enter completions browsing mode" in {
    val repl = newRepl
    repl.input("where").complete().complete()
    val Some(completionState: BrowserCompletionState) = repl.state.completionStateOpt
  }

  "Completion bug after a hyphen" should "not happen" in {
    val repl = newRepl
    repl.input("ls -42 # foo").left(8)
    repl.lineBuffer should equal(parseLineBuffer("ls -▶42 # foo"))

    repl.complete()

    repl.lineBuffer should equal(parseLineBuffer("ls -▶42 # foo"))
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

    repl.lineBuffer should equal(parseLineBuffer("▶23"))
  }

  "Repl" should "respect bare words setting" in {
    val repl = newRepl
    repl.input(s"config.${Config.Language.BareWords} = true").acceptLine()
    repl.input("foo").acceptLine()
    repl.it should equal(MashString("foo"))

    repl.input(s"config.${Config.Language.BareWords} = false").acceptLine()
    repl.input("foo").acceptLine()
    repl.it should equal(MashUnit /* Repl should have emitted an error */ )
  }

  "Type inference loop bug" should "not happen" in {
    val repl = newRepl
    repl.input("a => a").acceptLine()
    repl.complete() // previously blew up here
  }

  "Local variables" should "not collide with global" in {
    val repl = newRepl
    repl.input("a = 0").acceptLine()
    repl.input("def setA n = { a = n }").acceptLine()
    repl.input("setA 42").acceptLine()
    repl.input("a").acceptLine()
    repl.it should equal(MashNumber(0))
  }

  "Completing dotfiles" should "not have a bug where the original input is truncated" in {
    val repl = makeRepl(MockFileSystem.of("/.dotfiles/.bashrc"))
    repl.input(""""/.dotfiles/".""").complete()
    repl.text should equal(""""/.dotfiles/."""") // bug was it was "."
  }

  private def newRepl = makeRepl()

}

object ReplTest {

  def makeRepl(fileSystem: FileSystem = new MockFileSystem) = {
    val history = new HistoryImpl(new InMemoryHistoryStorage())
    new Repl(DummyTerminal(), NullPrintStream, fileSystem, new MockEnvironmentInteractions, history = history)
  }

  implicit class RichRepl(repl: Repl) {

    import com.github.mdr.mash.repl.NormalActions._

    def input(s: String): Repl = { repl.handleAction(SelfInsert(s)); repl }

    def complete(): Repl = { repl.handleAction(Complete); repl }

    def previousHistory(): Repl = { repl.handleAction(PreviousHistory); repl }

    def nextHistory(): Repl = { repl.handleAction(NextHistory); repl }

    def acceptLine(): Repl = { repl.handleAction(AcceptLine); repl }

    def toggleQuote(): Repl = { repl.handleAction(ToggleQuote); repl }

    def text: String = lineBuffer.text

    def lineBuffer: LineBuffer = repl.state.lineBuffer

    def left(n: Int = 1): Repl = {
      for (i ← 1 to n)
        repl.handleAction(BackwardChar)
      repl
    }

    def delete(): Repl = { repl.handleAction(DeleteChar); repl }

    def backspace(): Repl = { repl.handleAction(BackwardDeleteChar); repl }

    def draw(): Repl = { repl.draw(); repl }

    def it: MashValue = { repl.state.globalVariables(ReplState.It) }

    def incrementalCompletionState = repl.state.completionStateOpt.collect {
      case ics: IncrementalCompletionState ⇒ ics
    }.getOrElse(throw new AssertionError("Not in incremental completion mode"))

  }
}

case class DummyTerminal(width: Int = 80) extends Terminal {

  override def info = TerminalInfo(width, 40)

}

object NullPrintStream extends PrintStream(new OutputStream {
  override def write(b: Int) { /* no-op */ }
})
