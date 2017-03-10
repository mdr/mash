package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.core.objectClass._

object ObjectClass extends MashClass("core.Object") {

  override val staticMethods = Seq(
    MergeStaticMethod)

  override val methods = Seq(
    BlessMethod,
    FieldsMethod,
    GetMethod,
    GrepMethod,
    HasFieldMethod,
    HoistMethod,
    MapMethod,
    UnblessMethod,
    WithFieldMethod,
    WhereMethod,
    WhereNotMethod)

  override def summaryOpt = Some("The class of all objects")

  override def parentOpt = Some(AnyClass)

}