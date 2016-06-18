package com.github.mdr.mash.input

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
