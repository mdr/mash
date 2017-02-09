package com.github.mdr.mash.ns.os

import java.nio.file.{ Files, Path }

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.pathClass._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.subprocesses.ProcessRunner
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

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
    PathClassReadMethod,
    PathClassReadLinesMethod,
    PathClassRenameByMethod,
    PathClassRenameToMethod,
    PathClassRunMethod,
    PathClassSegmentsMethod,
    PathClassSizeMethod,
    PathClassTypeMethod,
    PathClassWriteMethod)

  object PathClassReadLinesMethod extends MashMethod("readLines") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val path = interpretAsPath(target)
      ReadLinesFunction.readLines(path)
    }

    override def typeInferenceStrategy = Type.Seq(StringClass)

    override def summaryOpt = Some("Read lines from this file")

    override def descriptionOpt = Some("""Returns a sequence of lines read from this file.
The default character encoding and line separator are used.""")

  }

  object PathClassReadMethod extends MashMethod("read") {
    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val path = interpretAsPath(target)
      MashString(fileSystem.read(path))
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("Read the contents of this file as a string")

    override def descriptionOpt = Some("""Returns a string with the contents of this file.
The default character encoding is used.""")

  }

  object PathClassRunMethod extends MashMethod("run") {

    object Params {
      val Args = Parameter(
        nameOpt = Some("args"),
        summaryOpt = Some("Arguments to command"),
        isVariadic = true)
    }
    import Params._

    val params = ParameterModel(Seq(Args))

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val args =
        target +: (boundParams(Args) match {
          case MashList(MashString(s, _)) ⇒ s.trim.split("\\s+").map(MashString(_)).toSeq
          case xs: MashList               ⇒ xs.elements
        })
      val result = ProcessRunner.runProcess(args, captureProcess = true)
      ProcessResultClass.fromResult(result)
    }

    override def typeInferenceStrategy = ProcessResultClass

    override def summaryOpt = Some("Execute the command at the given path, with the given arguments")

  }

  object PathClassAbsoluteMethod extends MashMethod("absolute") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val path = interpretAsPath(target)
      asPathString(fileSystem.pwd.resolve(path).toRealPath())
    }

    override def typeInferenceStrategy = StringClass taggedWith PathClass

    override def summaryOpt = Some("The absolute path to this location")

  }

  object PathClassParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val parent = interpretAsPath(target).getParent
      if (parent == null)
        MashNull
      else
        asPathString(parent)
    }

    override def typeInferenceStrategy = StringClass taggedWith PathClass

    override def summaryOpt = Some("The parent of this path")
  }

  object PathClassCopyIntoMethod extends MashMethod("copyInto") {

    object Params {
      val Destination = Parameter(
        nameOpt = Some("destination"),
        summaryOpt = Some("Location to copy file to"))
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: MashValue, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val source = interpretAsPath(target)
      val destination = boundParams.validatePath(Destination)
      if (!Files.isDirectory(destination))
        throw new EvaluatorException(s"Cannot copy into $destination, not a directory")
      if (Files.isDirectory(source))
        FileUtils.copyDirectoryToDirectory(source.toFile, destination.toFile)
      else
        FileUtils.copyFileToDirectory(source.toFile, destination.toFile)
      asPathString(destination.resolve(source.getFileName))
    }

    override def typeInferenceStrategy = StringClass taggedWith PathClass

    override def summaryOpt = Some("Copy this path into another location")

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
      Seq(CompletionSpec.Directory)

  }

  object PathClassMoveIntoMethod extends MashMethod("moveInto") {

    object Params {
      val Destination = Parameter(
        nameOpt = Some("destination"),
        summaryOpt = Some("Location to copy file to"))
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: MashValue, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val source = interpretAsPath(target)
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

    override def typeInferenceStrategy = StringClass taggedWith PathClass

    override def summaryOpt = Some("Move this path into the given directory")

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      Seq(CompletionSpec.Directory)

  }

  object PathClassDeleteMethod extends MashMethod("delete") {

    override def aliases = Seq("rm")

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val path = interpretAsPath(target)
      if (Files.isDirectory(path))
        FileUtils.deleteDirectory(path.toFile)
      else
        Files.delete(path)
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summaryOpt = Some("Delete this path")

  }

  object PathClassNameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      asPathString(interpretAsPath(target).getFileName)
    }

    override def typeInferenceStrategy = StringClass taggedWith PathClass

    override def summaryOpt = Some("Name (last segment) of this path")

  }

  object PathClassIsDirectoryMethod extends MashMethod("isDirectory") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Files.isDirectory(interpretAsPath(target)))
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Check if path is a directory")

  }

  object PathClassIsEmptyDirMethod extends MashMethod("isEmptyDir") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      val path = interpretAsPath(target)
      MashBoolean(Files.isDirectory(path) && fileSystem.getChildren(path, ignoreDotFiles = false, recursive = false).isEmpty)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Check if path is an empty directory")

  }

  object PathClassIsFileMethod extends MashMethod("isFile") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Files.isRegularFile(interpretAsPath(target)))
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Check if path is a directory")

  }

  object PathClassLastModifiedMethod extends MashMethod("lastModified") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashWrapped = {
      params.validate(arguments)
      MashWrapped(fileSystem.getPathSummary(interpretAsPath(target)).lastModified)
    }

    override def typeInferenceStrategy = DateTimeClass

    override def summaryOpt = Some("Last time path was modified")

  }

  object PathClassOwnerMethod extends MashMethod("owner") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(interpretAsPath(target))
      MashString(summary.owner, Some(UsernameClass))
    }

    override def typeInferenceStrategy = StringClass taggedWith UsernameClass

    override def summaryOpt = Some("Owner of this path")

  }

  object PathClassGroupMethod extends MashMethod("group") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(interpretAsPath(target))
      MashString(summary.group, Some(GroupClass))
    }

    override def typeInferenceStrategy = StringClass taggedWith GroupClass

    override def summaryOpt = Some("Group owner of this path")

  }

  object PathClassPermissionsMethod extends MashMethod("permissions") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(interpretAsPath(target))
      val permissions = summary.permissions
      PermissionsClass.asMashObject(permissions)
    }

    override def typeInferenceStrategy = PermissionsClass

    override def summaryOpt = Some("Permissions for this path")

  }

  object PathClassSizeMethod extends MashMethod("size") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(interpretAsPath(target))
      MashNumber(summary.size, Some(BytesClass))
    }

    override def typeInferenceStrategy = NumberClass taggedWith BytesClass

    override def summaryOpt = Some("Size of the file at this path")

  }

  object PathClassTypeMethod extends MashMethod("type") {

    private val fileSystem = LinuxFileSystem

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(interpretAsPath(target))
      MashString(summary.fileType, Some(FileTypeClass))
    }

    override def typeInferenceStrategy = StringClass taggedWith FileTypeClass

    override def summaryOpt = Some("Type of object at this path (file, directory etc)")

  }

  object PathClassSegmentsMethod extends MashMethod("segments") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val segments: Seq[Path] = interpretAsPath(target).asScala.toSeq
      MashList(segments.map(asPathString))
    }

    override def typeInferenceStrategy = Seq(StringClass)

    override def summaryOpt = Some("A sequence of the segments of this path (the parts of the path separated by /)")

  }

  override def summaryOpt = Some("Tag class for a filesystem path")

  override def parentOpt = Some(AnyClass)

}