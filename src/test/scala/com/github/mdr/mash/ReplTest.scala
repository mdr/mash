package com.github.mdr.mash

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.terminal.TerminalInfo

object DummyTerminal extends Terminal {

  override def info = TerminalInfo(80, 40)

}

class ReplTest extends FlatSpec with Matchers {

  "repl" should "worky" ignore {
    val repl = new Repl(DummyTerminal)
  }

}