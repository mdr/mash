package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.ns.collections.listClass.FunctionWrappingMethod
import com.github.mdr.mash.ns.core.objectClass._

object ObjectClass extends MashClass("core.Object") {

  override val staticMethods = Seq(
    FromPairsStaticMethod,
    MergeStaticMethod)

  override val methods = Seq(
    BlessMethod,
    FieldsMethod,
    GetMethod,
    GrepMethod,
    HasFieldMethod,
    HoistMethod,
    MapMethod,
    TransformFieldsMethod,
    TransformValuesMethod,
    UnblessMethod,
    WithFieldMethod,
    WhereMethod,
    WhereNotMethod,
    methodise(AllButLastFunction),
    methodise(FirstFunction),
    methodise(FlatMapFunction),
    methodise(LastFunction),
    methodise(ReverseFunction),
    methodise(SkipFunction),
    methodise(SortFunction),
    methodise(TransposeFunction))

  override def summaryOpt = Some("The class of all objects")

  override def parentOpt = Some(AnyClass)

  def methodise(function: MashFunction, methodAliases: Seq[String] = Seq()): MashMethod =
    FunctionWrappingMethod(function, methodAliases, isShy = true)

}