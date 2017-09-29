package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.pathClass._

object PathClass extends MashClass("os.Path") {

  override val methods: Seq[MashMethod] = Seq(
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
    OpenMethod,
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
    WithTildeMethod,
    TypeMethod,
    WriteMethod)

  override def summaryOpt = Some("Tag class for a filesystem path")

  override def parentOpt = Some(AnyClass)

}