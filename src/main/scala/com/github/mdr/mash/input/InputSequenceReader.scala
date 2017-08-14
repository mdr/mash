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
    "^[[1;2C" → shift(Key.Right),
    "^[[1;2D" → shift(Key.Left),
    "^[[1;3A" → alt(Key.Up),
    "^[[1;4C" → altShift(Key.Right),
    "^[[1;4D" → altShift(Key.Left),
    "^[^[[C" → altShift(Key.Right),
    "^[^[[D" → altShift(Key.Left),
    "^[[1;5C" → control(Key.Right),
    "^[[1;5D" → control(Key.Left),
    "^[[3~" → KeyPress(Delete),
    "^[[5~" → KeyPress(PageUp),
    "^[[6~" → KeyPress(PageDown),
    "^[[A" → KeyPress(Key.Up),
    "^[[B" → KeyPress(Key.Down),
    "^[[C" → KeyPress(Key.Right),
    "^[[D" → KeyPress(Key.Left),
    "^[[Z" → shift(Tab),
    "^[[H" → KeyPress(Home),
    "^[[F" → KeyPress(End),
    ("^[" + Del) → alt(Backspace),
    "^[b" → alt('b'),
    "^[d" → alt('d'),
    "^[e" → alt('e'),
    "^[f" → alt('f'),
    "^[w" → alt('w'),
    "^[B" → altShift('b'),
    "^[F" → altShift('f'),
    "^[OH" → KeyPress(Home),
    "^[OF" → KeyPress(End))

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
