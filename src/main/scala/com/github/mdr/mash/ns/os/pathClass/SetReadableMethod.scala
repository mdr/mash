package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermission

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

import scala.collection.JavaConverters._

object SetReadableMethod extends MashMethod("setReadable") {

  private val fileSystem = LinuxFileSystem

  import SetExecutableMethod.Params._

  val params = ParameterModel(Owner, Group, Others, All, Value)

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val path = FunctionHelpers.interpretAsPath(target)
    val value = boundParams(Value).isTruthy
    val parties = SetExecutableMethod.getParties(boundParams)

    val existingPermissions = Files.getPosixFilePermissions(path).asScala.toSet
    val newPermissionSet = calculateNewPermissionSet(value, parties, existingPermissions)
    Files.setPosixFilePermissions(path, newPermissionSet.asJava)

    val summary = fileSystem.getPathSummary(path)
    PermissionsClass.asMashObject(summary.permissions)
  }

  private def calculateNewPermissionSet(value: Boolean,
                                        parties: Parties,
                                        existingPermissionSet: Set[PosixFilePermission]): Set[PosixFilePermission] =
    if (value)
      existingPermissionSet ++ parties.readPermissionSet
    else
      existingPermissionSet diff parties.readPermissionSet

  override def typeInferenceStrategy = PermissionsClass

  override def summaryOpt = Some("Set the read bit on the given path")

  override def descriptionOpt = SetExecutableMethod.descriptionOpt.map(_.replaceAll("setExecutable", "setReadable"))

}
