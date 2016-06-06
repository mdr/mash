package com.github.mdr.mash.runtime

object MashNull extends MashValue {

  override def toString = "null"

  def option(x: MashValue): Option[MashValue] = x match {
    case MashNull ⇒ None
    case _        ⇒ Some(x)
  }

}