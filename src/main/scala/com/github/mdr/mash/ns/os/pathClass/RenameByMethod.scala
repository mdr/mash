package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.{ Files, Paths }

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object RenameByMethod extends MashMethod("renameBy") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function to transform the old name into a new name"))
  }

  import Params._

  val params = ParameterModel(F)

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val path = FunctionHelpers.interpretAsPath(target)
    val renamerFunction = boundParams.validateFunction(F)
    val newFileName = Paths.get(ToStringifier.stringify(renamerFunction(asPathString(path.getFileName))))
    RenameToMethod.validateName(newFileName, boundParams, F)
    val newPath = path.resolveSibling(newFileName)
    val newLocation = Files.move(path, newPath)
    asPathString(newLocation)
  }

  object RenameByMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      for {
        functionType ← argBindings.getType(F)
        targetType ← targetTypeOpt
      } inferencer.applyFunction(functionType, targetType)
      Some(StringClass taggedWith PathClass)
    }
  }

  override def typeInferenceStrategy = RenameByMethodTypeInferenceStrategy

  override def summaryOpt = Some("Rename this path using a function to transform the name")

}