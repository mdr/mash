package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.pathClass._
import org.apache.commons.lang3.SystemUtils

object PathClass extends MashClass("os.Path") {

  override val methods = Seq(
    AbsoluteMethod,
    BaseNameMethod,
    ChangeDirectoryMethod,
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
    CreateDirectoryMethod,
    MoveIntoMethod,
    NameMethod,
    OwnerMethod,
    ParentMethod,
    PermissionsMethod,
    ReadMethod,
    ReadLinesMethod,
    RenameByMethod,
    RenameToMethod,
    RunMethod,
    SegmentsMethod,
    SetExecutableMethod,
    SetReadableMethod,
    SetWritableMethod,
    SizeMethod,
    TypeMethod,
    WriteMethod) ++
    (if (SystemUtils.IS_OS_MAC_OSX)
      Seq(OpenMethod)
    else
      Seq())

  override def summaryOpt = Some("Tag class for a filesystem path")

  override def parentOpt = Some(AnyClass)

}