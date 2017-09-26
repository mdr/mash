package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.runtime.{ MashString, _ }

sealed trait SequenceLike

object SequenceLike {

  case class Object(obj: MashObject) extends SequenceLike

  case class String(s: MashString) extends SequenceLike {

    def items = s.characterSequence

    def reassemble(items: Seq[MashValue]) =
      if (items forall (_.isAString))
        items.asInstanceOf[Seq[MashString]].fold(MashString("", s.tagClassOpt))(_ + _)
      else
        MashList(items)
  }

  case class List(items: Seq[MashValue]) extends SequenceLike {
    def reassemble(items: Seq[MashValue]) = MashList(items)
  }

}

object SequenceLikeAnalyser {

  def analyse(boundParams: BoundParams, sequenceParam: Parameter)(f: SequenceLike ⇒ MashValue): MashValue = {
    val sequenceLike = boundParams(sequenceParam) match {
      case obj: MashObject ⇒
        ToListHelper.tryToList(obj) match {
          case Some(items) ⇒ SequenceLike.List(items)
          case None        ⇒ SequenceLike.Object(obj)
        }
      case s: MashString   ⇒ SequenceLike.String(s)
      case xs: MashList    ⇒ SequenceLike.List(xs.immutableElements)
      case value           ⇒
        boundParams.throwInvalidArgument(sequenceParam, s"Must be a List, String, or Object, but was a ${value.typeName}")
    }
    f(sequenceLike)
  }

}
