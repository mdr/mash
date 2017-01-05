package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.{ FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.os.ChildrenFunction
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object PathClassChildrenMethod extends MashMethod("children") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel(ChildrenFunction.params.params.tail)

  def apply(target: MashValue, arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val ignoreDotFiles = boundParams(ChildrenFunction.Params.IgnoreDotFiles).isTruthy
    val recursive = boundParams(ChildrenFunction.Params.Recursive).isTruthy
    val parentDir = FunctionHelpers.interpretAsPath(target)
    if (!fileSystem.exists(parentDir))
      throw new EvaluatorException(s"'$parentDir' does not exist")
    if (!fileSystem.isDirectory(parentDir))
      throw new EvaluatorException(s"'$parentDir' is not a directory")
    MashList(ChildrenFunction.getChildren(parentDir, ignoreDotFiles, recursive))
  }

  override def typeInferenceStrategy = new MethodTypeInferenceStrategy() {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val newArguments = TypedArguments(arguments.arguments :+ TypedArgument.PositionArg(AnnotatedExpr(None, targetTypeOpt)))
      ChildrenFunction.typeInferenceStrategy.inferTypes(inferencer, newArguments)
    }
  }

  override def summary = "The children of this path"

}