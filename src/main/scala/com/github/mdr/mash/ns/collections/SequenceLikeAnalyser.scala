package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.{ Inferencer, TypeInferenceStrategy, _ }
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.ns.core.{ CharacterClass, ObjectClass }
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

sealed trait SequenceLike

object SequenceLike {

  case class Object(obj: MashObject) extends SequenceLike

  case class String(s: MashString) extends SequenceLike {

    def characterSequence = s.s.toSeq.map(c ⇒ MashString(c.toString, Some(CharacterClass)))

    def reassemble(items: Seq[MashValue]) =
      if (items forall (_.isAString))
        items.asInstanceOf[Seq[MashString]].fold(MashString("", s.tagClassOpt))(_ + _)
      else
        MashList(items)
  }

  case class Items(items: Seq[MashValue]) extends SequenceLike

}

object SequenceLikeAnalyser {

  def analyse(boundParams: BoundParams, sequenceParam: Parameter)(f: SequenceLike ⇒ MashValue): MashValue = {
    boundParams(sequenceParam) match {
      case obj: MashObject ⇒
        ToListHelper.tryToList(obj) match {
          case Some(items) ⇒ f(SequenceLike.Items(items))
          case None        ⇒ f(SequenceLike.Object(obj))
        }
      case s: MashString   ⇒ f(SequenceLike.String(s))
      case xs: MashList    ⇒ f(SequenceLike.Items(xs.immutableElements))
      case value           ⇒
        boundParams.throwInvalidArgument(sequenceParam, s"Must be a List, String, or Object, but was a ${value.typeName}")
    }
  }

}
