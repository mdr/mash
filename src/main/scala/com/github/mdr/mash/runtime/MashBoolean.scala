package com.github.mdr.mash.runtime

sealed trait MashBoolean extends MashValue {
  import MashBoolean._

  def value = this == True

  def negate = this match {
    case True  ⇒ False
    case False ⇒ True
  }

  override def toString = this match {
    case True  ⇒ "true"
    case False ⇒ "false"
  }

}

object MashBoolean {
  case object True extends MashBoolean
  case object False extends MashBoolean

  def apply(x: Boolean): MashBoolean = if (x) True else False

}
