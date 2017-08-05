package com.github.mdr.mash.input

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.utils.PrefixTree

object InputSequenceReader {

  import InputSequence._

  private def makeEscapeTree(xs: (String, KeyPress)*): PrefixTree[KeyPress] =
    PrefixTree(xs.map { case (s, k) ⇒ s.drop(2) → k }: _*)

  private def alt(key: Key): KeyPress = KeyPress(key, alt = true)
  private def alt(c: Char): KeyPress = alt(Key.BasicKey(c))
  private def shift(key: Key): KeyPress = KeyPress(key, shift = true)
  private def shift(c: Char): KeyPress = shift(Key.BasicKey(c))
  private def altShift(c: Char): KeyPress = KeyPress(Key.BasicKey(c), shift = true, alt = true)
  private def control(key: Key): KeyPress = KeyPress(key, control = true)
  private def control(c: Char): KeyPress = control(Key.BasicKey(c))

  private val Esc = '\u001b'
  private val Del = '\u007f'

  private val EscapeSequenceTree = makeEscapeTree(
    "^[." → alt('.'),
    "^[," → alt(','),
    "^[[1;2D" → shift(Key.Left),
    "^[[1;2C" → shift(Key.Right),
    "^[[1;5C" → alt(Key.Right),
    "^[[1;5D" → alt(Key.Left),
    "^[[3~" → KeyPress(Key.Delete),
    "^[[5~" → KeyPress(Key.PageUp),
    "^[[6~" → KeyPress(Key.PageDown),
    "^[[A" → KeyPress(Key.Up),
    "^[[B" → KeyPress(Key.Down),
    "^[[C" → KeyPress(Key.Right),
    "^[[D" → KeyPress(Key.Left),
    "^[[Z" → shift(Key.Tab),
    "^[[H" → KeyPress(Key.Home),
    "^[[F" → KeyPress(Key.End),
    ("^[" + Del) → alt(Key.Backspace),
    "^[d" → alt('d'),
    "^[f" → alt('f'),
    "^[b" → alt('b'),
    "^[F" → altShift('f'),
    "^[B" → altShift('b'),
    "^[OH" → KeyPress(Key.Home),
    "^[OF" → KeyPress(Key.End))

  private def readChar(): Char = System.in.read().asInstanceOf[Char]

  def fetchInputSequence(): InputSequence =
    if (Singletons.terminalWindowChanged) {
      Singletons.terminalWindowChanged = false
      InputSequence.TerminalWindowChanged
    } else
      readChar() match {
        case Esc ⇒
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
        case '\u000B'   ⇒ control('k')
        case '\u000C'   ⇒ control('l')
        case '\u000E'   ⇒ control('n')
        case '\u0010'   ⇒ control('p')
        case '\u0011'   ⇒ control('q')
        case '\u0012'   ⇒ control('r')
        case '\u0014'   ⇒ control('t')
        case '\u0015'   ⇒ control('u')
        case '\u0016'   ⇒ control('v')
        case '\u0019'   ⇒ control('y')
        case '\t'       ⇒ KeyPress(Key.Tab)
        case '\n'       ⇒ KeyPress(Key.Enter)
        case '\r'       ⇒ KeyPress(Key.Enter)
        case '\b' | Del ⇒ KeyPress(Key.Backspace)
        case c          ⇒ OtherSequence(c + "")
      }

}
