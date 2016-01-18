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

object OldDirsFunction extends MashFunction("os.oldDirs") {

  private val workingDirectoryStack = Singletons.workingDirectoryStack

  val params = ParameterModel()

  def apply(arguments: Arguments): Seq[MashString] = {
    val boundParams = params.validate(arguments)
    workingDirectoryStack.oldDirs.map(FunctionHelpers.asPathString)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Tagged(StringClass, PathClass)))

  override def summary = "Previous working directories"

}