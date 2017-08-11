package com.github.mdr.mash.input

import scala.language.implicitConversions
import com.github.mdr.mash.input.InputSequence.KeyPress
import com.github.mdr.mash.input.Key.BasicKey

sealed trait InputSequence

object KeyDsl {

  implicit def charToBasicKey(c: Char): BasicKey = BasicKey(c)

  def control(c: Char) = KeyPress(c, control = true)

  def control(key: Key) = KeyPress(key, control = true)

  def alt(c: Char) = KeyPress(c, alt = true)

  def alt(key: Key) = KeyPress(key, alt = true)

  def altShift(c: Char) = KeyPress(c, alt = true, shift = true)

  def altShift(key: Key) = KeyPress(key, alt = true, shift = true)

  def shift(key: Key) = KeyPress(key, shift = true)

  implicit class RichChar(c: Char) {
    def control = KeyPress(c, control = true)
  }

}

object InputSequence {

  import KeyDsl._

  val ControlD = control('d')

  case class KeyPress(key: Key, shift: Boolean = false, control: Boolean = false, alt: Boolean = false) extends InputSequence

  case class EscapeSequence(s: String) extends InputSequence

  case class OtherSequence(s: String) extends InputSequence

  case object TerminalWindowChanged extends InputSequence

}
