package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.PermissionsSection
import scala.collection.immutable.ListMap
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashBoolean

object PermissionsSectionClass extends MashClass("os.PermissionsSection") {

  object Fields {
    val CanRead = Field("canRead", "Can read", Type.Instance(BooleanClass))
    val CanWrite = Field("canWrite", "Can write", Type.Instance(BooleanClass))
    val CanExecute = Field("canExecute", "Can execute", Type.Instance(BooleanClass))
  }

  import Fields._

  override val fields = Seq(CanRead, CanWrite, CanExecute)

  def asMashObject(section: PermissionsSection): MashObject = {
    val PermissionsSection(canRead, canWrite, canExecute) = section
    MashObject(ListMap(
      CanRead -> MashBoolean(canRead),
      CanWrite -> MashBoolean(canWrite),
      CanExecute -> MashBoolean(canExecute)),
      PermissionsSectionClass)
  }

  override def summary = "File permissions for particular class of user (owner, group or other)"

}

