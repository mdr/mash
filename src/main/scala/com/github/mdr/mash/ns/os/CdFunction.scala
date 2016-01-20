package com.github.mdr.mash.ns.os

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.Posix
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.ns.core.UnitClass

object CdFunction extends MashFunction("os.cd") {

  private val fileSystem = LinuxFileSystem
  private val environmentInteractions = LinuxEnvironmentInteractions
  private val workingDirectoryStack = Singletons.workingDirectoryStack

  object Params {
    val Directory = Parameter(
      name = "directory",
      summary = "Directory to change into; defaults to the current user's home directory.",
      defaultValueGeneratorOpt = Some(() ⇒ home))
  }
  import Params._

  val params = ParameterModel(Seq(Directory))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(boundParams(Directory))
    changeDirectory(path)
  }

  private def home = MashString(environmentInteractions.home.toString, Some(PathClass))

  def changeDirectory(path: Path) {
    workingDirectoryStack.push(fileSystem.pwd)
    if (fileSystem.isDirectory(path))
      fileSystem.chdir(path)
    else
      throw new EvaluatorException(s"Could not change directory to '$path', not a directory")
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.Directory)

  override def summary = "Change the current working directory"

  override def descriptionOpt = Some("""Examples:
   cd "/tmp"
   cd # cd to home directory""")

}