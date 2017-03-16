package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt
import scala.collection.immutable.ListMap

object GroupByFunction extends MashFunction("collections.groupBy") {

  private val DefaultTotalKeyName = "Total"

  object Params {
    val Discriminator = Parameter(
      nameOpt = Some("discriminator"),
      summaryOpt = Some("Function to apply to elements of the sequence to determine a key"))
    val Total = Parameter(
      nameOpt = Some("total"),
      summaryOpt = Some("Include a total group containing all the results"),
      shortFlagOpt = Some('t'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      flagValueNameOpt = Some("key"),
      descriptionOpt = Some(
        s"""If true, include an additional group containing all the elements.
If false (the default), this group is not included.
If a non-boolean argument is given, that is used as the key for the additional group.
Otherwise, a default key of "$DefaultTotalKeyName" is used. """))
    val IncludeNull = Parameter(
      nameOpt = Some("includeNull"),
      summaryOpt = Some("Include groups that have null keys"),
      shortFlagOpt = Some('n'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      flagValueNameOpt = Some("key"),
      descriptionOpt = Some(
        """If true, include a group with null keys, if any elements exist for such a group.
If false (the default), exclude a group with a null key.
If a non-boolean argument is given, that will be used as the key for the null group instead of null."""))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence from which to form groups"),
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(Discriminator, Total, IncludeNull, Sequence))

  def apply(boundParams: BoundParams): MashList = {
    val sequence = boundParams.validateSequence(Sequence)
    val discriminator = boundParams.validateFunction(Discriminator)
    val includeNulls = boundParams(IncludeNull).isTruthy
    val includeTotalGroup = boundParams(Total).isTruthy

    val nullKey = boundParams(IncludeNull) match {
      case MashBoolean.True ⇒ MashNull
      case v                ⇒ v
    }
    def translateKey(k: MashValue) = k match {
      case MashNull ⇒ nullKey
      case _        ⇒ k
    }
    var groups =
      for {
        (key, values) ← sequence.groupBy(discriminator).toSeq
        if key != MashNull || includeNulls
        groupKey = translateKey(key)
      } yield makeGroup(groupKey, values)

    if (includeTotalGroup) {
      val totalKey = boundParams(Total) match {
        case MashBoolean.True ⇒ MashString(DefaultTotalKeyName)
        case x                ⇒ x
      }
      val totalGroup = makeGroup(totalKey, sequence)
      groups = groups :+ totalGroup
    }

    MashList(groups)
  }

  private def makeGroup(key: MashValue, values: Seq[MashValue]) = {
    import GroupClass.Fields._
    MashObject.of(
      ListMap(
        Key -> key,
        Values -> MashList(values)),
      GroupClass)
  }

  override def typeInferenceStrategy = GroupByTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    MapFunction.getCompletionSpecs(argPos, arguments)

  override def summaryOpt = Some("Group together the elements of a sequence sharing a common key")

  override def descriptionOpt = Some(
    """Returns a sequence of Group objects, where each group contains
a subset of the sequence  sharing the same key, as determined by the given 
discriminator function.

Example:
  groupBy first ["foo", "bar", "baz"]
  ╔═══╤═════╤════════╗
  ║key│count│values  ║
  ╟───┼─────┼────────╢
  ║b  │2    │bar, baz║
  ║f  │1    │foo     ║
  ╚═══╧═════╧════════╝""")

}

object GroupByTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import GroupByFunction.Params._
    val argBindings = GroupByFunction.params.bindTypes(arguments)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val discriminatorExprOpt = argBindings.getArgument(Discriminator)
    for {
      keyType ← MapTypeInferenceStrategy.inferMappedType(inferencer, discriminatorExprOpt, sequenceTypeOpt)
      sequenceType ← sequenceTypeOpt
      valuesType ← condOpt(sequenceType) {
        case Type.Seq(elementType)      ⇒ elementType
        case Type.Patterns.AnyString(_) ⇒ sequenceType
      }
    } yield Type.Seq(Type.Generic(GroupClass, keyType, valuesType))
  }

}