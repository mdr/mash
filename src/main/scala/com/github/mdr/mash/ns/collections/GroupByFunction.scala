package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object GroupByFunction extends MashFunction("collections.groupBy") {

  private val DefaultTotalKeyName = "Total"

  object Params {
    val Discriminator = Parameter(
      nameOpt = Some("discriminator"),
      summaryOpt = Some("Function to apply to elements of the sequence to determine a key"))
    val Total = Parameter(
      nameOpt = Some("total"),
      summaryOpt = Some("Include a total group containing all the results (default false)"),
      shortFlagOpt = Some('t'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      flagValueNameOpt = Some("key"),
      descriptionOpt = Some(
        s"""If true, include an additional group containing all the elements.
If false, this group is not included.
If a non-boolean argument is given, that is used as the key for the additional group.
Otherwise, a default key of "$DefaultTotalKeyName" is used. """))
    val IncludeNull = Parameter(
      nameOpt = Some("includeNull"),
      summaryOpt = Some("Include groups that have null keys (default false)"),
      shortFlagOpt = Some('n'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      flagValueNameOpt = Some("key"),
      descriptionOpt = Some(
        """If true, include a group with null keys, if any elements exist for such a group.
If false, exclude a group with a null key.
If a non-boolean argument is given, that will be used as the key for the null group instead of null."""))
    val Object = Parameter(
      nameOpt = Some("object"),
      summaryOpt = Some("Output an Object with a field for each group (default false)"),
      shortFlagOpt = Some('o'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      descriptionOpt = Some(
        """If true, output an object with a field for each group, using the key as the field name, and
          |  the matching values as the field value.""".stripMargin))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence from which to form groups"))
  }

  import Params._

  val params = ParameterModel(Discriminator, Total, IncludeNull, Object, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val sequence = boundParams.validateSequence(Sequence)
    val discriminator = boundParams.validateFunction(Discriminator)
    val includeNulls = boundParams(IncludeNull).isTruthy
    val includeTotalGroup = boundParams(Total).isTruthy
    val outputAnObject = boundParams(Object).isTruthy

    val nullKey = boundParams(IncludeNull) match {
      case MashBoolean.True ⇒ MashNull
      case v                ⇒ v
    }
    def translateKey(k: MashValue) = k match {
      case MashNull ⇒ nullKey
      case _        ⇒ k
    }
    if (outputAnObject)
      MashObject.of(for {
        (key, values) ← sequence.groupBy(discriminator)
        stringKey = key match {
          case s: MashString ⇒ s.s
          case x             ⇒ throw new EvaluatorException(s"Group keys must be strings when grouping into an Object, but was a ${x.typeName}")
        }
      } yield stringKey -> MashList(values))
    else {
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
    s"""Returns a sequence of Group objects, where each group contains
a subset of the sequence  sharing the same key, as determined by the given 
discriminator function.

An Object can be output instead, if the ${Object.name} flag is true.

Example:
  groupBy first ["foo", "bar", "baz"]
  ╔═══╤═════╤════════╗
  ║key│count│values  ║
  ╟───┼─────┼────────╢
  ║b  │2    │bar, baz║
  ║f  │1    │foo     ║
  ╚═══╧═════╧════════╝""")

}
