package com.github.mdr.mash.input

import com.github.mdr.mash.repl.NormalActions._

trait InputAction

object InputAction {

  def fetchAction(isLineEmpty: Boolean, keyMap: KeyMap): InputAction = {
    val inputSequence = InputSequenceReader.fetchInputSequence()
    inputSequenceToAction(inputSequence, isLineEmpty, keyMap)
  }

  private def inputSequenceToAction(inputSequence: InputSequence, isLineEmpty: Boolean, keyMap: KeyMap): InputAction =
    if (inputSequence == InputSequence.ControlD && isLineEmpty)
      EndOfFile
    else
      keyMap.map.getOrElse(inputSequence, inputSequence match {
        case InputSequence.OtherSequence(s) ⇒ SelfInsert(s)
        case _                              ⇒ Noop
      })

}

