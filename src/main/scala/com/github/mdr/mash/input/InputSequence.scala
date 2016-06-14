package com.github.mdr.mash.input

import com.github.mdr.mash.utils.PrefixTree

sealed trait InputSequence

object InputSequence {

  val ControlD = KeyPress(Key.BasicKey('d'), control = true)

  case class KeyPress(key: Key, shift: Boolean = false, control: Boolean = false, alt: Boolean = false) extends InputSequence
  case class EscapeSequence(s: String) extends InputSequence
  case class OtherSequence(s: String) extends InputSequence
  case object TerminalWindowChanged extends InputSequence
}
