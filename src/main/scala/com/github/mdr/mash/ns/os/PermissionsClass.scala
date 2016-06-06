package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.PathSummary
import scala.collection.immutable.ListMap
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.os.Permissions
import com.github.mdr.mash.runtime.MashObject

object PermissionsClass extends MashClass("os.Permissions") {

  object Fields {
    val Owner = Field("owner", "Owner permissions", Type.Instance(PermissionsSectionClass))
    val Group = Field("group", "Group permissions", Type.Instance(PermissionsSectionClass))
    val Others = Field("others", "Others permissions", Type.Instance(PermissionsSectionClass))
  }

  import Fields._

  override val fields = Seq(Owner, Group, Others)

  def asMashObject(permissions: Permissions): MashObject = {
    val Permissions(owner, others, group) = permissions
    MashObject(ListMap(
      Owner -> PermissionsSectionClass.asMashObject(owner),
      Group -> PermissionsSectionClass.asMashObject(group),
      Others -> PermissionsSectionClass.asMashObject(others)),
      PermissionsClass)
  }

  override def summary = "File permissions"

}