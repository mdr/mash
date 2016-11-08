package com.github.mdr.mash.ns.os

import java.nio.file.Files
import java.nio.file.Path

import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.FunctionHelpers.asPathString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.pathClass._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.subprocesses.ProcessRunner

object PathClass extends MashClass("os.Path") {

  override val methods = Seq(
    PathClassAbsoluteMethod,
    PathClassBaseNameMethod,
    PathClassCdMethod,
    PathClassChildrenMethod,
    PathClassCopyIntoMethod,
    PathClassCopyMethod,
    PathClassDeleteMethod,
    PathClassExistsMethod,
    PathClassExtensionMethod,
    PathClassFollowLinkMethod,
    PathClassGroupMethod,
    PathClassInfoMethod,
    PathClassIsDirectoryMethod,
    PathClassIsEmptyDirMethod,
    PathClassIsFileMethod,
    PathClassLastModifiedMethod,
    PathClassCreateDirectoryMethod,
    PathClassMoveIntoMethod,
    PathClassNameMethod,
    PathClassOwnerMethod,
    PathClassParentMethod,
    PathClassPermissionsMethod,
    PathClassReadLinesMethod,
    PathClassRenameByMethod,
    PathClassRenameToMethod,
    PathClassRunMethod,
    PathClassSegmentsMethod,
    PathClassSizeMethod,
    PathClassTypeMethod,
    MashClass.alias("rm", PathClassDeleteMethod))

  object PathClassReadLinesMethod extends MashMethod("readLines") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      ReadLinesFunction.readLines(path)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

    override def summary = "Read lines from the file"

    override def descriptionOpt = Some("""Returns a sequence of lines read from the given file.
The default character encoding and line separator are used.""")

  }

  object PathClassRunMethod extends MashMethod("run") {

    object Params {
      val Args = Parameter(
        name = "args",
        summary = "Arguments to command",
        isVariadic = true)
    }
    import Params._

    val params = ParameterModel(Seq(Args))

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val args =
        target +: (boundParams(Args) match {
          case MashList(MashString(s, _)) ⇒ s.trim.split("\\s+").map(MashString(_)).toSeq
          case xs: MashList               ⇒ xs.items
        })
      val result = ProcessRunner.runProcess(args, captureProcess = true)
      ProcessResultClass.fromResult(result)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ProcessResultClass)

    override def summary = "Execute the command at the given path, with the given arguments"

  }

  object PathClassAbsoluteMethod extends MashMethod("absolute") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      asPathString(fileSystem.pwd.resolve(path).toRealPath())
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

    override def summary = "The absolute path to this location"

  }

  object PathClassParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val parent = FunctionHelpers.interpretAsPath(target).getParent
      if (parent == null)
        MashNull
      else
        asPathString(parent)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

    override def summary = "The parent of this path"
  }

  object PathClassCopyIntoMethod extends MashMethod("copyInto") {

    object Params {
      val Destination = Parameter(
        name = "destination",
        summary = "Location to copy file to")
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: MashValue, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val source = FunctionHelpers.interpretAsPath(target)
      val destination = boundParams.validatePath(Destination)
      if (!Files.isDirectory(destination))
        throw new EvaluatorException(s"Cannot copy into $destination, not a directory")
      if (Files.isDirectory(source))
        FileUtils.copyDirectoryToDirectory(source.toFile, destination.toFile)
      else
        FileUtils.copyFileToDirectory(source.toFile, destination.toFile)
      asPathString(destination.resolve(source.getFileName))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

    override def summary = "Copy this path into another location"

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
      Seq(CompletionSpec.Directory)

  }

  object PathClassMoveIntoMethod extends MashMethod("moveInto") {

    object Params {
      val Destination = Parameter(
        name = "destination",
        summary = "Location to copy file to")
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: MashValue, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val source = FunctionHelpers.interpretAsPath(target)
      val destination = boundParams.validatePath(Destination)
      if (!Files.isDirectory(destination))
        throw new EvaluatorException(s"Cannot copy into $destination, not a directory")
      val newPath =
        if (Files.isDirectory(source))
          FileUtils.moveDirectoryToDirectory(source.toFile, destination.toFile, false)
        else
          FileUtils.moveFileToDirectory(source.toFile, destination.toFile, false)
      asPathString(destination.resolve(source.getFileName))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

    override def summary = "Move this path into the given directory"

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      Seq(CompletionSpec.Directory)

  }

  object PathClassDeleteMethod extends MashMethod("delete") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      if (Files.isDirectory(path))
        FileUtils.deleteDirectory(path.toFile)
      else
        Files.delete(path)
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Delete this path"

  }

  object PathClassNameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      asPathString(FunctionHelpers.interpretAsPath(target).getFileName)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

    override def summary = "Name (last segment) of this path"

  }

  object PathClassIsDirectoryMethod extends MashMethod("isDirectory") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Files.isDirectory(FunctionHelpers.interpretAsPath(target)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if path is a directory"

  }

  object PathClassIsEmptyDirMethod extends MashMethod("isEmptyDir") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      MashBoolean(Files.isDirectory(path) && fileSystem.getChildren(path, ignoreDotFiles = false, recursive = false).isEmpty)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if path is an empty directory"

  }

  object PathClassIsFileMethod extends MashMethod("isFile") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Files.isRegularFile(FunctionHelpers.interpretAsPath(target)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if path is a directory"

  }

  object PathClassLastModifiedMethod extends MashMethod("lastModified") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      MashWrapped(fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target)).lastModified)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(DateTimeClass)

    override def summary = "Last time path was modified"

  }

  object PathClassOwnerMethod extends MashMethod("owner") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashString(summary.owner, Some(UsernameClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith UsernameClass)

    override def summary = "Owner of this path"

  }

  object PathClassGroupMethod extends MashMethod("group") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashString(summary.group, Some(GroupClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith GroupClass)

    override def summary = "Group owner of this path"

  }

  object PathClassPermissionsMethod extends MashMethod("permissions") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      val permissions = summary.permissions
      PermissionsClass.asMashObject(permissions)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(PermissionsClass)

    override def summary = "Permissions for this path"

  }

  object PathClassSizeMethod extends MashMethod("size") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashNumber(summary.size, Some(BytesClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass taggedWith BytesClass)

    override def summary = "Size of the file at this path"

  }

  object PathClassTypeMethod extends MashMethod("type") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashString(summary.fileType, Some(FileTypeClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith FileTypeClass)

    override def summary = "Type of object at this path (file, directory etc)"

  }

  object PathClassSegmentsMethod extends MashMethod("segments") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val segments: Seq[Path] = FunctionHelpers.interpretAsPath(target).asScala.toSeq
      MashList(segments.map(FunctionHelpers.asPathString))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(StringClass))

    override def summary = "A sequence of the segments of this path (the parts of the path separated by /)"

  }

  override def summary = "Tag class for a filesystem path"

  override def parentOpt = Some(AnyClass)

}