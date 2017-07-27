package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.os.Permissions
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

import scala.collection.immutable.ListMap

object PermissionsClass extends MashClass("os.Permissions") {

  object Fields {
    val Owner = Field("owner", Some("Owner permissions"), PermissionsSectionClass)
    val Group = Field("group", Some("Group permissions"), PermissionsSectionClass)
    val Others = Field("others", Some("Others permissions"), PermissionsSectionClass)
  }

  import Fields._

  override val fields = Seq(Owner, Group, Others)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(x: MashValue) extends AbstractObjectWrapper(x) {

    def owner: MashObject = getObjectField(Owner)
    def group: MashObject = getObjectField(Group)
    def others: MashObject = getObjectField(Others)

  }

  def asMashObject(permissions: Permissions): MashObject = {
    val Permissions(owner, group, others) = permissions
    MashObject.of(ListMap(
      Owner -> PermissionsSectionClass.asMashObject(owner),
      Group -> PermissionsSectionClass.asMashObject(group),
      Others -> PermissionsSectionClass.asMashObject(others)),
      PermissionsClass)
  }

  override def summaryOpt = Some("File permissions")

}