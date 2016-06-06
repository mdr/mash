package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import scala.PartialFunction.condOpt
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.inference.Inferencer
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull

object FirstFunction extends MashFunction("collections.first") {

  object Params {
    val N: Parameter = Parameter(
      name = "n",
      summary = "Number of elements",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to find the first value(s) of",
      isLast = true)
  }
  import Params._

  val params = ParameterModel(Seq(N, Sequence))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    boundParams.validateSequence(Sequence)
    val sequence = boundParams(Sequence)
    val countOpt = boundParams.validateIntegerOrNull(N)
    countOpt match {
      case Some(n) ⇒
        sequence match {
          case s: MashString ⇒ s.modify(_.take(n))
          case xs: MashList  ⇒ xs.take(n)
        }
      case None ⇒
        sequence match {
          case s: MashString ⇒
            if (s.isEmpty) MashNull else s.first
          case xs: MashList ⇒
            if (xs.isEmpty) MashNull else xs.head
        }
    }
  }

  override def typeInferenceStrategy = FirstTypeInferenceStrategy

  override def summary = "Find the first element(s) of a sequence"

  override def descriptionOpt = Some(s"""If a count ${N.name} is provided, the first ${N.name} items of the sequence will be returned.
If there are fewer than ${N.name} in the sequence, the entire sequence is returned.
If a count ${N.name} is omitted, then the first item of the sequence is returned, if nonempty, else null.

Examples:
   first 3 [1, 2, 3, 4 5] # [1, 2, 3]
   first 5 [1, 2, 3]      # [1, 2, 3]
   first [1, 2, 3]        # 1
   first []               # null""")

}

object FirstTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FirstFunction.params.bindTypes(arguments)
    import FirstFunction.Params._
    if (argBindings.contains(N))
      for {
        AnnotatedExpr(_, sequenceTypeOpt) ← argBindings.get(Sequence)
        sequenceType ← sequenceTypeOpt
      } yield sequenceType
    else
      for {
        AnnotatedExpr(_, sequenceTypeOpt) ← argBindings.get(Sequence)
        sequenceType ← sequenceTypeOpt
        elementType ← condOpt(sequenceType) {
          case Type.Seq(elementType)                                    ⇒ elementType
          case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒ sequenceType
        }
      } yield elementType
  }

}