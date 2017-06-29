package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap

object GroupByFunction extends MashFunction("collections.groupBy") {

  private val DefaultTotalKeyName = "Total"

  object Params {
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
    val Groups = Parameter(
      nameOpt = Some("groups"),
      summaryOpt = Some("Output a List of Group objects (default false)"),
      shortFlagOpt = Some('g'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true,
      descriptionOpt = Some(
        """If true, output a list of Group objects, one for each group. If false (the default), output a single
          | object with a field for each group.
        """.stripMargin))
    val Discriminator = Parameter(
      nameOpt = Some("discriminator"),
      summaryOpt = Some("Function to apply to elements of the sequence to determine a key"))
    val Select = Parameter(
      nameOpt = Some("select"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      isFlag = true,
      isFlagValueMandatory = true,
      summaryOpt = Some("Function to apply to elements of the sequence to determine the final values of the output groups"))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence from which to form groups"))
  }

  import Params._

  val params = ParameterModel(Total, IncludeNull, Groups, Discriminator, Select, Sequence)

  private def groupBy[T, U](xs: Seq[T], f: T ⇒ U): Seq[(U, Seq[T])] = {
    val map = LinkedHashMap[U, Seq[T]]()
    for (x ← xs) {
      val group = f(x)
      map += group -> (map.getOrElse(group, Seq()) :+ x)
    }
    map.toSeq
  }

  def call(boundParams: BoundParams): MashValue = {
    val sequence = boundParams.validateSequence(Sequence)
    val discriminator = boundParams.validateFunction(Discriminator)
    val select: MashValue ⇒ MashValue = boundParams.validateFunctionOpt(Select).getOrElse(identity _)
    val includeNulls = boundParams(IncludeNull).isTruthy
    val includeTotalGroup = boundParams(Total).isTruthy
    val outputListOfGroups = boundParams(Groups).isTruthy

    val nullKey = boundParams(IncludeNull) match {
      case MashBoolean.True ⇒ MashNull
      case v                ⇒ v
    }

    val totalKey = boundParams(Total) match {
      case MashBoolean.True ⇒ MashString(DefaultTotalKeyName)
      case x                ⇒ x
    }

    def translateKey(k: MashValue) = k match {
      case MashNull ⇒ nullKey
      case _        ⇒ k
    }

    var groupsByKey: Seq[(MashValue, Seq[MashValue])] =
      for {
        (key, values) ← groupBy(sequence, discriminator)
        if key != MashNull || includeNulls
        groupKey = translateKey(key)
      } yield groupKey -> (values map select)
    if (includeTotalGroup)
      groupsByKey ++= Seq(totalKey -> (sequence map select))

    if (outputListOfGroups)
      MashList(groupsByKey.map((makeGroupObject _).tupled))
    else
      MashObject.of(groupsByKey.map { case (k, g) ⇒ k -> MashList(g) })
  }

  private def makeGroupObject(key: MashValue, values: Seq[MashValue]): MashObject = {
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

If the --${Groups.name} flag is true, an Object is output instead, with a field for every group key.
--total cannot be used with --object.

Example:
  groupBy first ["foo", "bar", "baz"]
  ╔═══╤═════╤════════╗
  ║key│count│values  ║
  ╟───┼─────┼────────╢
  ║b  │2    │bar, baz║
  ║f  │1    │foo     ║
  ╚═══╧═════╧════════╝""")

}
