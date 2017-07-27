package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermission

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.utils.Utils._

import scala.collection.JavaConverters._

object SetExecutableMethod extends MashMethod("setExecutable") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val All = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Set the executable bit for all (owner, group and others) (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Owner = Parameter(
      nameOpt = Some("owner"),
      summaryOpt = Some("Set the executable bit for the owner (default false)"),
      shortFlagOpt = Some('u'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Group = Parameter(
      nameOpt = Some("group"),
      summaryOpt = Some("Set the executable bit for the group (default false)"),
      shortFlagOpt = Some('g'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Others = Parameter(
      nameOpt = Some("others"),
      summaryOpt = Some("Set the executable bit for others (default false)"),
      shortFlagOpt = Some('o'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("The value for the executable bit (default true)"),
      defaultValueGeneratorOpt = Some(true))
  }

  import Params._

  val params = ParameterModel(Owner, Group, Others, All, Value)

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val path = FunctionHelpers.interpretAsPath(target)
    val value = boundParams(Value).isTruthy
    val all = boundParams(All).isTruthy
    val owner = boundParams(Owner).isTruthy
    val group = boundParams(Group).isTruthy
    val others = boundParams(Others).isTruthy
    val existingPermissions = Files.getPosixFilePermissions(path).asScala.toSet

    val newPermissions = calculateNewPermissions(value, all, owner, group, others, existingPermissions)

    Files.setPosixFilePermissions(path, newPermissions.asJava)

    val summary = fileSystem.getPathSummary(path)
    PermissionsClass.asMashObject(summary.permissions)
  }

  private def calculateNewPermissions(value: Boolean,
                                      all: Boolean,
                                      owner: Boolean,
                                      group: Boolean,
                                      others: Boolean,
                                      existingPermissions: Set[PosixFilePermission]): Set[PosixFilePermission] = {
    val updatedPermissions = Set(
      (owner || all).option(PosixFilePermission.OWNER_EXECUTE),
      (group || all).option(PosixFilePermission.GROUP_EXECUTE),
      (others || all).option(PosixFilePermission.OTHERS_EXECUTE)).flatten
    val newPermissions =
      if (value)
        existingPermissions ++ updatedPermissions
      else
        existingPermissions diff updatedPermissions
    newPermissions
  }

  override def typeInferenceStrategy = PermissionsClass

  override def summaryOpt = Some("Set the executable bit on the given file")

}
