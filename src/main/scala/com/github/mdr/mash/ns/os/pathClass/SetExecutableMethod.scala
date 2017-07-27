package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermission

import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.utils.Utils._

import scala.collection.JavaConverters._

object SetExecutableMethod extends MashMethod("setExecutable") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val All: Parameter = Parameter(
      nameOpt = Some("all"),
      summaryOpt = Some("Set the bit for all (owner, group and others) (default false)"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue),
      isBooleanFlag = true)
    val Owner: Parameter = Parameter(
      nameOpt = Some("owner"),
      summaryOpt = Some("Set the bit for the owner (default false)"),
      shortFlagOpt = Some('u'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue),
      isBooleanFlag = true)
    val Group: Parameter = Parameter(
      nameOpt = Some("group"),
      summaryOpt = Some("Set the bit for the group (default false)"),
      shortFlagOpt = Some('g'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue),
      isBooleanFlag = true)
    val Others: Parameter = Parameter(
      nameOpt = Some("others"),
      summaryOpt = Some("Set the bit for others (default false)"),
      shortFlagOpt = Some('o'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue),
      isBooleanFlag = true)
    val Value: Parameter = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("The value for the bit (default true)"),
      defaultValueGeneratorOpt = Some(true))
  }

  import Params._

  val params = ParameterModel(Owner, Group, Others, All, Value)

  private case class Parties(owner: Boolean, group: Boolean, others: Boolean) {

    def permissionSet = Set(
      owner.option(PosixFilePermission.OWNER_EXECUTE),
      group.option(PosixFilePermission.GROUP_EXECUTE),
      others.option(PosixFilePermission.OTHERS_EXECUTE)).flatten

  }

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val path = FunctionHelpers.interpretAsPath(target)
    val value = boundParams(Value).isTruthy
    val parties = getParties(boundParams)

    val existingPermissions = Files.getPosixFilePermissions(path).asScala.toSet
    val newPermissionSet = calculateNewPermissionSet(value, parties, existingPermissions)
    Files.setPosixFilePermissions(path, newPermissionSet.asJava)

    val summary = fileSystem.getPathSummary(path)
    PermissionsClass.asMashObject(summary.permissions)
  }

  private def getParties(boundParams: BoundParams): Parties = {
    val allOpt = NoArgFunction.option(boundParams(All)).map(_.isTruthy)
    val ownerOpt = NoArgFunction.option(boundParams(Owner)).map(_.isTruthy)
    val groupOpt = NoArgFunction.option(boundParams(Group)).map(_.isTruthy)
    val othersOpt = NoArgFunction.option(boundParams(Others)).map(_.isTruthy)

    val all = allOpt getOrElse false
    val owner = all || (ownerOpt getOrElse false) || Seq(allOpt, ownerOpt, groupOpt, othersOpt).flatten.isEmpty
    val group = all || (groupOpt getOrElse false)
    val others = all || (othersOpt getOrElse false)
    Parties(owner, group, others)
  }

  private def calculateNewPermissionSet(value: Boolean,
                                        parties: Parties,
                                        existingPermissionSet: Set[PosixFilePermission]): Set[PosixFilePermission] =
    if (value)
      existingPermissionSet ++ parties.permissionSet
    else
      existingPermissionSet diff parties.permissionSet

  override def typeInferenceStrategy = PermissionsClass

  override def summaryOpt = Some("Set the execute/search bit on the given path")

  override def descriptionOpt = Some(
    s"""If none of the --$Owner, --$Group, --$Others or --$All flags are specified, then the owner bit is changed.
        |
        |Returns the updated permissions for the path.
        |
        |Examples:
        |<mash>
        |  path.setExecutable                  # Set permission for the owner to true
        |  path.setExecutable false            # Set permission for the owner to true
        |  path.setExecutable --group --others # Set permission for group and others to true
        |  path.setExecutable -og              # Set permission for group and others to true
        |  path.setExecutable --all false      # Set permission for everyone to false
        |</mash>""".stripMargin)

  override def exampleTargetName = "path"

}
