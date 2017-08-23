package com.github.mdr.mash.input

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.utils.PrefixTree
import com.github.mdr.mash.input.KeyDsl._
import com.github.mdr.mash.input.Key.{ Left ⇒ _, Right ⇒ _, _ }
import com.github.mdr.mash.utils.CharUtils._

object InputSequenceReader {

  import InputSequence._

  private def makeEscapeTree(escapes: (String, KeyPress)*): PrefixTree[KeyPress] =
    PrefixTree(escapes.map { case (escape, key) ⇒ escape.replace("^[", Esc.toString).tail → key }: _*)

  private val EscapeSequenceTree = makeEscapeTree(
    "^[[1;2A" → shift(Key.Up), // Gnome terminal, xterm
    "^[[1;2B" → shift(Key.Down), // Gnome terminal, xterm
    "^[[1;2D" → shift(Key.Left), // Gnome terminal, xterm
    "^[[1;2C" → shift(Key.Right), // Gnome terminal, xterm
    "^[[a" → shift(Key.Up), // rxvt
    "^[[b" → shift(Key.Down), // rxvt
    "^[[d" → shift(Key.Left), // rxvt
    "^[[c" → shift(Key.Right), // rxvt
    "^[[1;3A" → alt(Key.Up), // Gnome terminal, xterm
    "^[[1;3B" → alt(Key.Down), // Gnome terminal, xterm
    "^[^[[A" → alt(Key.Up), // iTerm, rxvt, putty
    "^[^[[B" → alt(Key.Down), // iTerm, rxvt, putty
    "^[[1;4C" → altShift(Key.Right), // xterm
    "^[[1;4D" → altShift(Key.Left), // xterm
    "^[^[[C" → altShift(Key.Right), // putty
    "^[^[[D" → altShift(Key.Left), // putty
    "^[[1;5C" → control(Key.Right), // xterm
    "^[[1;5D" → control(Key.Left), // xterm
    "^[[3~" → KeyPress(Delete), // Linux console, xterm, Gnome terminal, putty
    "^[[5~" → KeyPress(PageUp), // Linux console, xterm, Gnome terminal, putty
    "^[[6~" → KeyPress(PageDown), // Linux console, xterm, Gnome terminal, putty
    "^[[A" → KeyPress(Key.Up), // Linux console, xterm, Gnome terminal, putty
    "^[[B" → KeyPress(Key.Down), // Linux console, xterm, Gnome terminal, putty
    "^[[C" → KeyPress(Key.Right), // Linux console, xterm, Gnome terminal, putty
    "^[[D" → KeyPress(Key.Left), // Linux console, xterm, Gnome terminal, putty
    "^[[Z" → shift(Tab), // xterm, Gnome terminal, putty
    "^[[7$" → shift(Home), // rxvt
    "^[[8$" → shift(End), // rxvt
    "^[[1;2H" → shift(Home), // Konsole, xterm
    "^[[1;2F" → shift(End), // Konsole, xterm
    "^[[1~" → KeyPress(Home), // Linux console, putty
    "^[[4~" → KeyPress(End), // Linux console, putty
    "^[[7~" → KeyPress(Home), // rxvt
    "^[[8~" → KeyPress(End), // rxvt
    "^[[H" → KeyPress(Home), // gnome-terminal, iTerm, xterm
    "^[[F" → KeyPress(End), // gnome-terminal, iTerm, xterm
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
    "^[B" → altShift('b'),
    "^[F" → altShift('f'),
    "^[\r" → alt(Enter))

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
        case '\u0000'   ⇒ shift(Space)
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
        case '\t'       ⇒ KeyPress(Tab)
        case '\n'       ⇒ KeyPress(Enter)
        case '\r'       ⇒ KeyPress(Enter)
        case '\b' | Del ⇒ KeyPress(Backspace)
        case c          ⇒ OtherSequence(c + "")
      }

}
