package com.github.mdr.mash.printer

import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.evaluator.Field

object PermissionsPrinter {

  def permissionsSectionString(section: MashObject): String = {
    val s = new StringBuilder
    def test(field: Field) = MemberEvaluator.lookup(section, field).asInstanceOf[Boolean]
    import PermissionsSectionClass.Fields
    if (test(Fields.CanRead)) s.append("r") else s.append("-")
    if (test(Fields.CanWrite)) s.append("w") else s.append("-")
    if (test(Fields.CanExecute)) s.append("x") else s.append("-")
    s.toString
  }

  def permissionsString(perms: MashObject): String = {
    import PermissionsClass.Fields
    val s = new StringBuilder
    val owner = MemberEvaluator.lookup(perms, Fields.Owner).asInstanceOf[MashObject]
    val group = MemberEvaluator.lookup(perms, Fields.Group).asInstanceOf[MashObject]
    val others = MemberEvaluator.lookup(perms, Fields.Others).asInstanceOf[MashObject]
    s.append(permissionsSectionString(owner))
    s.append(permissionsSectionString(group))
    s.append(permissionsSectionString(others))
    s.toString
  }

}