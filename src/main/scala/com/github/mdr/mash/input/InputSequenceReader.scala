package com.github.mdr.mash.input

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.input.KeyDsl._
import com.github.mdr.mash.utils.PrefixTree
import com.github.mdr.mash.utils.CharUtils._

object InputSequenceReader {

  import InputSequence._

  private def makeEscapeTree(escapes: (String, KeyPress)*): PrefixTree[KeyPress] =
    PrefixTree(escapes.map { case (escape, key) ⇒ escape.replace("^[", Esc.toString).tail → key }: _*)

  private val EscapeSequenceTree = {
    import Key._
    makeEscapeTree(
      "^[[1;2A" → shift(Up), // Gnome terminal, xterm, iTerm
      "^[[1;2B" → shift(Down), // Gnome terminal, xterm, iTerm
      "^[[1;2C" → shift(Right), // Gnome terminal, xterm, Terminal.app, iTerm
      "^[[1;2D" → shift(Left), // Gnome terminal, xterm, Terminal.app, iTerm
      "^[[2A" → shift(Up), // intellij
      "^[[2B" → shift(Down), // intellij
      "^[[2C" → shift(Right), // intellij
      "^[[2D" → shift(Left), // intellij
      "^[[a" → shift(Up), // rxvt
      "^[[b" → shift(Down), // rxvt
      "^[[c" → shift(Right), // rxvt
      "^[[d" → shift(Left), // rxvt
      "^[[1;3A" → alt(Up), // Gnome terminal, xterm
      "^[[1;3B" → alt(Down), // Gnome terminal, xterm
      "^[^[[A" → alt(Up), // iTerm, rxvt, putty, Terminal.app, intellij
      "^[^[[B" → alt(Down), // iTerm, rxvt, putty, Terminal.app, intellij
      "^[^[[C" → alt(Right), // Terminal.app, intellij
      "^[^[[D" → alt(Left), // Terminal.app, intellij
      "^[[1;4C" → altShift(Right), // xterm
      "^[[1;4D" → altShift(Left), // xterm
      "^[[1;10C" → altShift(Right), // iTerm
      "^[[1;10D" → altShift(Left), // iTerm
      "^[[1;5C" → control(Right), // xterm, Terminal.app, iTerm
      "^[[1;5D" → control(Left), // xterm, Terminal.app, iTerm
      "^[[5C" → control(Right), // intellij
      "^[[5D" → control(Left), // intellij
      "^[[3~" → KeyPress(Delete), // Linux console, xterm, Gnome terminal, putty, Terminal.app, intellij, iTerm
      "^[[5~" → KeyPress(PageUp), // Linux console, xterm, Gnome terminal, putty, iTerm, intellij
      "^[[6~" → KeyPress(PageDown), // Linux console, xterm, Gnome terminal, putty, iTerm, intellij
      "^[[A" → KeyPress(Up), // Linux console, xterm, Gnome terminal, putty, Terminal.app, intellij
      "^[[B" → KeyPress(Down), // Linux console, xterm, Gnome terminal, putty, Terminal.app, intellij
      "^[[C" → KeyPress(Right), // Linux console, xterm, Gnome terminal, putty, Terminal.app, intellij
      "^[[D" → KeyPress(Left), // Linux console, xterm, Gnome terminal, putty, Terminal.app, intellij
      "^[[Z" → shift(Tab), // xterm, Gnome terminal, putty, Terminal.app, iTerm
      "^[[7$" → shift(Home), // rxvt
      "^[[8$" → shift(End), // rxvt
      "^[[1;2H" → shift(Home), // Konsole, xterm, iTerm
      "^[[1;2F" → shift(End), // Konsole, xterm, iTerm
      "^[[2H" → shift(Home), // intellij
      "^[[2F" → shift(End), // intellij
      "^[[1~" → KeyPress(Home), // Linux console, putty
      "^[[4~" → KeyPress(End), // Linux console, putty
      "^[[7~" → KeyPress(Home), // rxvt
      "^[[8~" → KeyPress(End), // rxvt
      "^[[H" → KeyPress(Home), // gnome-terminal, iTerm, xterm, intellij
      "^[[F" → KeyPress(End), // gnome-terminal, iTerm, xterm, intellij
      "^[OH" → KeyPress(Home), // older Gnome terminal? https://bugzilla.gnome.org/show_bug.cgi?id=600659
      "^[OF" → KeyPress(End), // older Gnome terminal?
      ("^[" + Del) → alt(Backspace),
      "^[." → alt('.'),
      "^[," → alt(','),
      "^[_" → alt('_'),
      "^[<" → alt('<'),
      "^[>" → alt('>'),
      "^[b" → alt('b'),
      "^[d" → alt('d'),
      "^[e" → alt('e'),
      "^[f" → alt('f'),
      "^[w" → alt('w'),
      "^[B" → altShift('b'), // terminal.app, iterm
      "^[F" → altShift('f'), // terminal.app, iterm
      "^[\r" → alt(Enter))
  }

  private def readChar(): Char = System.in.read().asInstanceOf[Char]

  def fetchInputSequence(): InputSequence =
    if (Singletons.terminalWindowChanged) {
      Singletons.terminalWindowChanged = false
      InputSequence.TerminalWindowChanged
    } else
      readChar() match {
        case Esc        ⇒
          EscapeSequenceTree.get(readChar()) match {
            case Left(chars)     ⇒ EscapeSequence(chars.mkString)
            case Right(keyPress) ⇒ keyPress
          }
        case '\u0000'   ⇒ shift(Key.Space)
        case '\u0001'   ⇒ control('a')
        case '\u0002'   ⇒ control('b')
        case '\u0004'   ⇒ control('d')
        case '\u0005'   ⇒ control('e')
        case '\u0006'   ⇒ control('f')
        case '\u0007'   ⇒ control('g')
        case '\u000B'   ⇒ control('k')
        case '\u000C'   ⇒ control('l')
        case '\u000E'   ⇒ control('n')
        case '\u0010'   ⇒ control('p')
        case '\u0011'   ⇒ control('q')
        case '\u0012'   ⇒ control('r')
        case '\u0014'   ⇒ control('t')
        case '\u0015'   ⇒ control('u')
        case '\u0016'   ⇒ control('v')
        case '\u0017'   ⇒ control('w')
        case '\u0018'   ⇒ control('x')
        case '\u0019'   ⇒ control('y')
        case '\u001F'   ⇒ control('_')
        case '\t'       ⇒ KeyPress(Key.Tab)
        case '\n'       ⇒ KeyPress(Key.Enter)
        case '\r'       ⇒ KeyPress(Key.Enter)
        case '\b' | Del ⇒ KeyPress(Key.Backspace)
        case c          ⇒ OtherSequence(c + "")
      }

}
