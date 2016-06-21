package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files
import java.nio.file.Paths

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference.Inferencer
import com.github.mdr.mash.inference.MethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue

object PathClassRenameByMethod extends MashMethod("renameBy") {

  object Params {
    val F = Parameter(
      name = "f",
      summary = "Function to transform the old name into a new name")
  }
  import Params._

  val params = ParameterModel(Seq(F))

  def apply(target: MashValue, arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    val renamerFunction = boundParams.validateFunction(F)
    val newFileName = Paths.get(ToStringifier.stringify(renamerFunction(asPathString(path.getFileName))))
    PathClassRenameToMethod.validateName(newFileName, boundParams, F)
    val newPath = path.resolveSibling(newFileName)
    val newLocation = Files.move(path, newPath)
    asPathString(newLocation)
  }

  override def typeInferenceStrategy = new MethodTypeInferenceStrategy {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      for {
        annotatedExpr ← argBindings.get(F)
        functionType ← annotatedExpr.typeOpt
        targetType ← targetTypeOpt
      } inferencer.applyFunction(functionType, targetType, None)
      Some(StringClass taggedWith PathClass)
    }
  }

  override def summary = "Rename this path using a function to transform the name"

}