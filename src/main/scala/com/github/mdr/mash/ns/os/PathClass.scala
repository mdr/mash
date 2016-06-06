package com.github.mdr.mash.ns.os

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.FilenameUtils
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import java.time.Instant
import java.nio.file.Path
import com.github.mdr.mash.functions.BoundParams
import com.github.mdr.mash.subprocesses.ProcessRunner
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashBoolean

object PathClass extends MashClass("os.Path") {

  private val fileSystem = LinuxFileSystem

  override val methods = Seq(
    AbsoluteMethod,
    BaseNameMethod,
    CdMethod,
    ChildrenMethod,
    CopyIntoMethod,
    CopyMethod,
    DeleteMethod,
    ExistsMethod,
    ExtensionMethod,
    FollowLinkMethod,
    GroupMethod,
    InfoMethod,
    IsDirectoryMethod,
    IsEmptyDirMethod,
    IsFileMethod,
    LastModifiedMethod,
    MkdirMethod,
    MoveIntoMethod,
    NameMethod,
    OwnerMethod,
    ParentMethod,
    PermissionsMethod,
    ReadLinesMethod,
    RenameByMethod,
    RenameToMethod,
    RunMethod,
    SegmentsMethod,
    SizeMethod,
    TypeMethod,
    MashClass.alias("rm", DeleteMethod))

  object ExistsMethod extends MashMethod("exists") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      MashBoolean(fileSystem.exists(path))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Whether or not an item exists at this location"

  }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      val summary = fileSystem.getPathSummary(path)
      PathSummaryClass.asMashObject(summary)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(PathSummaryClass)

    override def summary = "Get PathSummary object for this path"

  }

  object MkdirMethod extends MashMethod("createDirectory") {

    import CreateDirectoryFunction.Params.CreateIntermediates

    val params = ParameterModel(Seq(CreateIntermediates))

    def apply(target: Any, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val createIntermediates = Truthiness.isTruthy(boundParams(CreateIntermediates))
      val path = FunctionHelpers.interpretAsPath(target)
      val resultPath = fileSystem.createDirectory(path, createIntermediates)
      asPathString(resultPath)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "Create directory at this path"

  }

  object FollowLinkMethod extends MashMethod("followLink") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      val resolved = Files.readSymbolicLink(path)
      MashString(resolved.toString, PathClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "Follow this symbolic link"

  }

  object CdMethod extends MashMethod("cd") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments) {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      CdFunction.changeDirectory(path)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Change directory to this path"

  }

  object ExtensionMethod extends MashMethod("extension") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val name = FunctionHelpers.interpretAsPath(target).getFileName.toString
      if (name contains ".")
        MashString(name.reverse.takeWhile(_ != '.').reverse)
      else
        MashNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "File extension, if any, else null"

  }

  object BaseNameMethod extends MashMethod("baseName") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val name = FunctionHelpers.interpretAsPath(target).getFileName.toString
      MashString(FilenameUtils.getBaseName(name))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = "Name without extension"

  }

  object ReadLinesMethod extends MashMethod("readLines") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      ReadLinesFunction.readLines(path)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

    override def summary = "Read lines from the file"

    override def descriptionOpt = Some("""Returns a sequence of lines read from the given file.
The default character encoding and line separator are used.""")

  }

  object RenameToMethod extends MashMethod("renameTo") {

    object Params {
      val NewName = Parameter(
        name = "newName",
        summary = "New name for the file or directory",
        descriptionOpt = Some("""New name must be a simple name (not a path with directory separators)"""))
    }
    import Params._

    val params = ParameterModel(Seq(NewName))

    def apply(target: Any, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      val newName = validateName(boundParams, NewName)
      val newPath = path.resolveSibling(newName)
      val newLocation = Files.move(path, newPath)
      asPathString(newLocation)
    }

    private def validateName(boundParams: BoundParams, param: Parameter): Path = {
      val newName = boundParams.validatePath(param)
      validateName(newName, boundParams, param)
    }

    def validateName(newName: Path, boundParams: BoundParams, param: Parameter): Path =
      if (newName.asScala.size > 1)
        boundParams.throwInvalidArgument(param, "Name cannot contain directory separators")
      else if (newName.toString == "")
        boundParams.throwInvalidArgument(param, "Name cannot be empty")
      else if (newName.toString == "." || newName.toString == "..")
        boundParams.throwInvalidArgument(param, "Name cannot be . or ..")
      else
        newName

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "Rename the file or directory at this path"

  }

  object RenameByMethod extends MashMethod("renameBy") {

    object Params {
      val F = Parameter(
        name = "f",
        summary = "Function to transform the old name into a new name")
    }
    import Params._

    val params = ParameterModel(Seq(F))

    def apply(target: Any, arguments: Arguments): MashString = {
      val boundParams = params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      val renamerFunction = boundParams.validateFunction(F)
      val newFileName = Paths.get(ToStringifier.stringify(renamerFunction(asPathString(path.getFileName))))
      RenameToMethod.validateName(newFileName, boundParams, F)
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
        Some(Type.Tagged(StringClass, PathClass))
      }
    }

    override def summary = "Rename this path using a function to transform the name"

  }

  object RunMethod extends MashMethod("run") {

    object Params {
      val Args = Parameter(
        name = "args",
        summary = "Arguments to command",
        isVariadic = true)
    }
    import Params._

    val params = ParameterModel(Seq(Args))

    def apply(target: Any, arguments: Arguments): MashObject = {
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

  object AbsoluteMethod extends MashMethod("absolute") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      asPathString(fileSystem.pwd.resolve(path).toRealPath())
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "The absolute path to this location"

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val parent = FunctionHelpers.interpretAsPath(target).getParent
      if (parent == null)
        MashNull
      else
        asPathString(parent)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "The parent of this path"
  }

  object ChildrenMethod extends MashMethod("children") {

    val params = ParameterModel(ChildrenFunction.params.params.tail)

    def apply(target: Any, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val ignoreDotFiles = Truthiness.isTruthy(boundParams(ChildrenFunction.Params.IgnoreDotFiles))
      val recursive = Truthiness.isTruthy(boundParams(ChildrenFunction.Params.Recursive))
      val parentDir = FunctionHelpers.interpretAsPath(target)
      if (!fileSystem.exists(parentDir))
        throw new EvaluatorException(s"'$parentDir' does not exist")
      if (!fileSystem.isDirectory(parentDir))
        throw new EvaluatorException(s"'$parentDir' is not a directory")
      MashList(ChildrenFunction.getChildren(parentDir, ignoreDotFiles, recursive))
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy() {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
        val newArguments = SimpleTypedArguments(arguments.arguments :+ TypedArgument.PositionArg(AnnotatedExpr(None, targetTypeOpt)))
        ChildrenFunction.typeInferenceStrategy.inferTypes(inferencer, newArguments)
      }
    }

    override def summary = "The children of this path"

  }

  object CopyMethod extends MashMethod("copy") {

    object Params {
      val Destination = Parameter(
        name = "destination",
        summary = "Location to copy file to")
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: Any, arguments: Arguments) {
      val boundParams = params.validate(arguments)
      val source = FunctionHelpers.interpretAsPath(target)
      val destination = boundParams.validatePath(Destination)
      if (Files.isDirectory(source))
        if (Files.exists(destination))
          throw new EvaluatorException("Destination already exists")
        else
          FileUtils.copyDirectory(source.toFile, destination.toFile)
      else {
        if (Files.exists(destination) && Files.isDirectory(destination))
          throw new EvaluatorException("Destination already exists, and is a directory")
        else
          Files.copy(source, destination)
      }
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Copy this file or directory to another location"

  }

  object CopyIntoMethod extends MashMethod("copyInto") {

    object Params {
      val Destination = Parameter(
        name = "destination",
        summary = "Location to copy file to")
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: Any, arguments: Arguments): MashString = {
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

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "Copy this path into another location"

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
      Seq(CompletionSpec.Directory)

  }

  object MoveIntoMethod extends MashMethod("moveInto") {

    object Params {
      val Destination = Parameter(
        name = "destination",
        summary = "Location to copy file to")
    }
    import Params._

    val params = ParameterModel(Seq(Destination))

    def apply(target: Any, arguments: Arguments): MashString = {
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

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "Move this path into the given directory"

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      Seq(CompletionSpec.Directory)

  }

  object DeleteMethod extends MashMethod("delete") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments) {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      if (Files.isDirectory(path))
        FileUtils.deleteDirectory(path.toFile)
      else
        Files.delete(path)

    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Delete this path"

  }

  object NameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      asPathString(FunctionHelpers.interpretAsPath(target).getFileName)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

    override def summary = "Name (last segment) of this path"

  }

  object IsDirectoryMethod extends MashMethod("isDirectory") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Files.isDirectory(FunctionHelpers.interpretAsPath(target)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if path is a directory"

  }

  object IsEmptyDirMethod extends MashMethod("isEmptyDir") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      val path = FunctionHelpers.interpretAsPath(target)
      MashBoolean(Files.isDirectory(path) && fileSystem.getChildren(path, ignoreDotFiles = false, recursive = false).isEmpty)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if path is an empty directory"

  }

  object IsFileMethod extends MashMethod("isFile") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      MashBoolean(Files.isRegularFile(FunctionHelpers.interpretAsPath(target)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Check if path is a directory"

  }

  object LastModifiedMethod extends MashMethod("lastModified") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Instant = {
      params.validate(arguments)
      fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target)).lastModified
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(DateTimeClass)

    override def summary = "Last time path was modified"

  }

  object OwnerMethod extends MashMethod("owner") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashString(summary.owner, Some(UsernameClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, UsernameClass))

    override def summary = "Owner of this path"

  }

  object GroupMethod extends MashMethod("group") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashString(summary.group, Some(GroupClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, GroupClass))

    override def summary = "Group owner of this path"

  }

  object PermissionsMethod extends MashMethod("permissions") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      val permissions = summary.permissions
      PermissionsClass.asMashObject(permissions)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(PermissionsClass)

    override def summary = "Permissions for this path"

  }

  object SizeMethod extends MashMethod("size") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashNumber(summary.size, Some(BytesClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(NumberClass, BytesClass))

    override def summary = "Size of the file at this path"

  }

  object TypeMethod extends MashMethod("type") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      val summary = fileSystem.getPathSummary(FunctionHelpers.interpretAsPath(target))
      MashString(summary.fileType, Some(FileTypeClass))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, FileTypeClass))

    override def summary = "Type of object at this path (file, directory etc)"

  }

  object SegmentsMethod extends MashMethod("segments") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
      params.validate(arguments)
      val segments: Seq[Path] = FunctionHelpers.interpretAsPath(target).asScala.toSeq
      MashList(segments.map(FunctionHelpers.asPathString))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

    override def summary = "A sequence of the segments of this path (the parts of the path separated by /)"

  }

  override def summary = "Tag class for a filesystem path"

}