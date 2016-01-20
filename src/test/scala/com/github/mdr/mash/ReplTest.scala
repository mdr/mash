package com.github.mdr.mash

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.terminal.TerminalInfo
import java.io.PrintStream
import java.io.OutputStream
import com.github.mdr.mash.evaluator.MashNumber

class ReplTest extends FlatSpec with Matchers {

  "Repl" should "work" in {
    val repl = new Repl(DummyTerminal, NullPrintStream)
    repl.handleAction(InputAction.SelfInsert('1'))
    repl.handleAction(InputAction.AcceptLine)
    repl.state.globalVariables("it") should equal(MashNumber(1))
  }

}

object DummyTerminal extends Terminal {

  override def info = TerminalInfo(80, 40)

}

object NullPrintStream extends PrintStream(new OutputStream() {
  override def write(b: Int) {}
})
