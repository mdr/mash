package com.github.mdr.mash.runtime

import scala.PartialFunction.condOpt

sealed trait MashBoolean extends MashValue with Comparable[MashBoolean] {
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

  def compareTo(that: MashBoolean) = (this, that) match {
    case (True, True) | (False, False) ⇒ 0
    case (False, True) ⇒ -1
    case (True, False) ⇒ 1
  }

}

object MashBoolean {

  case object True extends MashBoolean
  case object False extends MashBoolean

  def apply(x: Boolean): MashBoolean = if (x) True else False

  def unapply(x: MashValue): Option[MashBoolean] = condOpt(x) {
    case True  ⇒ True
    case False ⇒ False
  }
}
