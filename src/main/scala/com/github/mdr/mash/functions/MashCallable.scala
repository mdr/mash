package com.github.mdr.mash.functions

import com.github.mdr.mash.classes.BoundMethod
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.runtime.MashValue

import scala.PartialFunction._

trait MashCallable {
  
  def name: String
  
}

object NullaryCallable {

  def unapply(value: MashValue): Option[NullaryCallable] =
    condOpt(value){
      case f: MashFunction if f.allowsNullary  ⇒ f
      case bm: BoundMethod if bm.allowsNullary ⇒ bm
    }

}

trait NullaryCallable {

  def callNullary(): MashValue

}