package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.ns.os.WriteFunction
import com.github.mdr.mash.runtime.{ MashBoolean, MashUnit, MashValue }

object WriteMethod extends MashMethod("write") {

  object Params {
    val Append = Parameter(
      nameOpt = Some("append"),
      summaryOpt = Some("Append to the end of the file, if it already exists"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to write to the file"),
      descriptionOpt = Some(
        """If the given data is a sequence, write a line to the file for each item.
Otherwise, write the item as a string."""))
  }

  import Params._

  val params = ParameterModel(Seq(Append, Data))

  def call(target: MashValue, boundParams: BoundParams): MashUnit = {
    val append = boundParams(Append).isTruthy
    val path = interpretAsPath(target)
    val data = boundParams(Data)
    WriteFunction.write(path, data, append)
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Write an object or sequence of objects to a file as a string")

  override def descriptionOpt = Some(
    """The default encoding is used to convert the strings to bytes.
If multiple lines are written, the default line separator is used.""")

}
