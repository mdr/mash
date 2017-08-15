package com.github.mdr.mash.input

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.utils.PrefixTree
import com.github.mdr.mash.input.KeyDsl._
import com.github.mdr.mash.input.Key.{ Left ⇒ _, Right ⇒ _, _ }

object InputSequenceReader {

  import InputSequence._

  private def makeEscapeTree(escapes: (String, KeyPress)*): PrefixTree[KeyPress] =
    PrefixTree(escapes.map { case (escape, key) ⇒ escape.replace("^[", Esc.toString).tail → key }: _*)

  private val Esc = '\u001b'
  private val Del = '\u007f'

  private val EscapeSequenceTree = makeEscapeTree(
    "^[." → alt('.'),
    "^[," → alt(','),
    "^[[1;2A" → shift(Key.Up),
    "^[[1;2B" → shift(Key.Down),
    "^[[1;2D" → shift(Key.Left),
    "^[[1;2C" → shift(Key.Right),
    "^[[a" → shift(Key.Up), // rxvt
    "^[[b" → shift(Key.Down), // rxvt
    "^[[d" → shift(Key.Left), // rxvt
    "^[[c" → shift(Key.Right), // rxvt
    "^[[1;3A" → alt(Key.Up), // Gnome terminal, xterm
    "^[[1;3B" → alt(Key.Down), // Gnome terminal, xterm
    "^[^[[A" → alt(Key.Up), // iTerm, rxvt
    "^[^[[B" → alt(Key.Down), // iTerm, rxvt
    "^[[1;4C" → altShift(Key.Right),
    "^[[1;4D" → altShift(Key.Left),
    "^[^[[C" → altShift(Key.Right),
    "^[^[[D" → altShift(Key.Left),
    "^[[1;5C" → control(Key.Right),
    "^[[1;5D" → control(Key.Left),
    "^[[3~" → KeyPress(Delete), // Linux console
    "^[[5~" → KeyPress(PageUp), // Linux console
    "^[[6~" → KeyPress(PageDown), // Linux console
    "^[[A" → KeyPress(Key.Up), // Linux console
    "^[[B" → KeyPress(Key.Down), // Linux console
    "^[[C" → KeyPress(Key.Right), // Linux console
    "^[[D" → KeyPress(Key.Left), // Linux console
    "^[[Z" → shift(Tab),
    "^[[7$" → shift(Home), // rxvt
    "^[[8$" → shift(End), // rxvt
    "^[[1;2H" → shift(Home), // Konsole/xterm
    "^[[1;2F" → shift(End), // Konsole/xterm
    "^[[1~" → KeyPress(Home), // Linux console
    "^[[4~" → KeyPress(End), // Linux console
    "^[[7~" → KeyPress(Home), // rxvt
    "^[[8~" → KeyPress(End), // rxvt
    "^[[H" → KeyPress(Home), // gnome-terminal, iTerm
    "^[[F" → KeyPress(End), // gnome-terminal, iTerm
    "^[OH" → KeyPress(Home),
    "^[OF" → KeyPress(End),
    ("^[" + Del) → alt(Backspace),
    "^[b" → alt('b'),
    "^[d" → alt('d'),
    "^[e" → alt('e'),
    "^[f" → alt('f'),
    "^[w" → alt('w'),
    "^[B" → altShift('b'),
    "^[F" → altShift('f'))

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
