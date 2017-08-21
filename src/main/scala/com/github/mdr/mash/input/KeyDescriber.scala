package com.github.mdr.mash.input

import com.github.mdr.mash.input.InputSequence.KeyPress
import com.github.mdr.mash.input.Key._
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.input.KeyDsl._

object KeyDescriber {

  def describe(keyPress: KeyPress): String =
    if (keyPress == shift(Tab))
      "⇤"
    else {
      val KeyPress(key, shift, control, alt) = keyPress
      Seq(
        shift.option("⇧"),
        control.option("⌃"),
        alt.option("⌥"),
        Some(describe(key))).flatten.mkString
    }

  def describe(key: Key): String = key match {
    case Enter       ⇒ "↩"
    case Space       ⇒ "␣"
    case Backspace   ⇒ "⌫"
    case Delete      ⇒ "␡"
    case PageUp      ⇒ "⇞"
    case PageDown    ⇒ "⇟"
    case Home        ⇒ "↖"
    case End         ⇒ "↘"
    case Escape      ⇒ "⎋"
    case Up          ⇒ "↑"
    case Down        ⇒ "↓"
    case Left        ⇒ "←"
    case Right       ⇒ "→"
    case Tab         ⇒ "⇥"
    case BasicKey(c) ⇒ c.toString
  }

}
