package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.attribute.PosixFilePermission
import com.github.mdr.mash.utils.Utils._

case class Parties(owner: Boolean, group: Boolean, others: Boolean) {

  def readPermissionSet = Set(
    owner.option(PosixFilePermission.OWNER_READ),
    group.option(PosixFilePermission.GROUP_READ),
    others.option(PosixFilePermission.OTHERS_READ)).flatten

  def writePermissionSet = Set(
    owner.option(PosixFilePermission.OWNER_WRITE),
    group.option(PosixFilePermission.GROUP_WRITE),
    others.option(PosixFilePermission.OTHERS_WRITE)).flatten

  def executePermissionSet = Set(
    owner.option(PosixFilePermission.OWNER_EXECUTE),
    group.option(PosixFilePermission.GROUP_EXECUTE),
    others.option(PosixFilePermission.OTHERS_EXECUTE)).flatten

}

