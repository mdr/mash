package com.github.mdr.mash

sealed trait Key

object Key {
  case class BasicKey(c: Character) extends Key
  case object Enter extends Key
  case object Space extends Key
  case object Backspace extends Key
  case object Delete extends Key
  case object PageUp extends Key
  case object PageDown extends Key
  case object Home extends Key
  case object End extends Key
  case object Escape extends Key
  case object Up extends Key
  case object Down extends Key
  case object Left extends Key
  case object Right extends Key
  case object Tab extends Key
}

sealed trait InputSequence

object InputSequence {

  case class KeyPress(key: Key, shift: Boolean = false, control: Boolean = false, alt: Boolean = false) extends InputSequence
  case class EscapeSequence(s: String) extends InputSequence
  case class OtherSequence(s: String) extends InputSequence

  import InputSequence._

  private def readChar(): Char = System.in.read().asInstanceOf[Char]

  def fetchInputSequence(): InputSequence =
    readChar() match {
      case '\u001b' ⇒ // ESC
        readChar() match {
          case '.' ⇒
            KeyPress(Key.BasicKey('.'), alt = true)
          case '[' ⇒ readChar() match {
            case '1' ⇒ readChar() match {
              case ';' ⇒ readChar() match {
                case '5' ⇒ readChar() match {
                  case 'C' ⇒ KeyPress(Key.Right, alt = true)
                  case 'D' ⇒ KeyPress(Key.Left, alt = true)
                  case c   ⇒ EscapeSequence("[1;5" + c)
                }
                case c ⇒ EscapeSequence("[1;" + c)
              }
              case c ⇒ EscapeSequence("[1" + c)
            }
            case '3' ⇒ readChar() match {
              case '~' ⇒ KeyPress(Key.Delete)
              case c   ⇒ EscapeSequence("[3" + c)
            }
            case '5' ⇒ readChar() match {
              case '~' ⇒ KeyPress(Key.PageUp)
              case c   ⇒ EscapeSequence("[5" + c)
            }
            case '6' ⇒ readChar() match {
              case '~' ⇒ KeyPress(Key.PageDown)
              case c   ⇒ EscapeSequence("[6" + c)
            }
            case 'A' ⇒ KeyPress(Key.Up)
            case 'B' ⇒ KeyPress(Key.Down)
            case 'C' ⇒ KeyPress(Key.Right)
            case 'D' ⇒ KeyPress(Key.Left)
            case 'Z' ⇒ KeyPress(Key.Tab, shift = true)
            case 'H' ⇒ KeyPress(Key.Home)
            case 'F' ⇒ KeyPress(Key.End)
            case c   ⇒ EscapeSequence("[" + c)
          }
          case '\u007f' ⇒ KeyPress(Key.Backspace, alt = true)
          case 'd'      ⇒ KeyPress(Key.BasicKey('d'), alt = true)
          case 'f'      ⇒ KeyPress(Key.BasicKey('f'), alt = true)
          case 'b'      ⇒ KeyPress(Key.BasicKey('b'), alt = true)
          case 'O' ⇒ readChar() match {
            case 'H' ⇒ KeyPress(Key.Home)
            case 'F' ⇒ KeyPress(Key.End)
            case c   ⇒ EscapeSequence("O" + c)
          }
          case c ⇒ EscapeSequence("" + c)
        }
      case '\u0000'        ⇒ KeyPress(Key.Space, shift = true)
      case '\u0001'        ⇒ KeyPress(Key.BasicKey('a'), control = true)
      case '\u0002'        ⇒ KeyPress(Key.BasicKey('b'), control = true)
      case '\u0005'        ⇒ KeyPress(Key.BasicKey('e'), control = true)
      case '\u0006'        ⇒ KeyPress(Key.BasicKey('f'), control = true)
      case '\u000B'        ⇒ KeyPress(Key.BasicKey('k'), control = true)
      case '\u000C'        ⇒ KeyPress(Key.BasicKey('l'), control = true)
      case '\u000E'        ⇒ KeyPress(Key.BasicKey('n'), control = true)
      case '\u0010'        ⇒ KeyPress(Key.BasicKey('p'), control = true)
      case '\u0011'        ⇒ KeyPress(Key.BasicKey('q'), control = true)
      case '\u0012'        ⇒ KeyPress(Key.BasicKey('r'), control = true)
      case '\t'            ⇒ KeyPress(Key.Tab)
      case '\u0004'        ⇒ KeyPress(Key.BasicKey('d'), control = true)
      case '\n'            ⇒ KeyPress(Key.Enter)
      case '\r'            ⇒ KeyPress(Key.Enter)
      case '\b' | '\u007f' ⇒ KeyPress(Key.Backspace)
      case c               ⇒ OtherSequence(c + "")
    }

}
