package com.github.mdr.mash.printer

import com.github.mdr.mash.ns.os.{ PermissionsClass, PermissionsSectionClass }
import com.github.mdr.mash.runtime.MashObject

object PermissionsPrinter {

  def permissionsSectionString(section: MashObject): String = {
    val sb = new StringBuilder
    val wrapper = PermissionsSectionClass.Wrapper(section)
    if (wrapper.canRead) sb.append("r") else sb.append("-")
    if (wrapper.canWrite) sb.append("w") else sb.append("-")
    if (wrapper.canExecute) sb.append("x") else sb.append("-")
    sb.toString
  }

  def permissionsString(perms: MashObject): String = {
    val wrapper = PermissionsClass.Wrapper(perms)
    val sb = new StringBuilder
    sb.append(permissionsSectionString(wrapper.owner))
    sb.append(permissionsSectionString(wrapper.group))
    sb.append(permissionsSectionString(wrapper.others))
    sb.toString
  }

}