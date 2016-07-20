package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.ArgumentException
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.Inferencer
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.os.WithinFunction.Params
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashValue

object WithinFunction extends MashFunction("os.within") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val Directory = Parameter(
      name = "directory",
      summary = "Directory to change into")
    val Block = Parameter(
      name = "block",
      summary = "Code to execute",
      isLazy = true)
  }
  import Params._

  val params = ParameterModel(Seq(Directory, Block))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(Directory)
    val f = boundParams(Block).asInstanceOf[MashFunction]
    val oldDir = fileSystem.pwd
    if (fileSystem.isDirectory(path))
      fileSystem.chdir(path)
    else
      throw new EvaluatorException(s"Could not change directory to '$path', not a directory")
    try
      f.apply(Arguments(Seq()))
    finally
      fileSystem.chdir(oldDir)
  }

  override def typeInferenceStrategy = WithinFunctionTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Directory ⇒ CompletionSpec.Directory
    }

  override def summary = "Temporarily change the current working directory to execute a task, then change back"

  override def descriptionOpt = Some("""Examples:
   within "~/project" git.log""")

}

object WithinFunctionTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import WithinFunction.Params._
    val argBindings = WithinFunction.params.bindTypes(arguments)
    argBindings.get(Block).flatMap(_.typeOpt)
  }

}