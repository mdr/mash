package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ BoundMethod, Field, MashClass }
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object HelpCreator {

  def getHelp(item: MashValue): MashObject = item match {
    case f: MashFunction  ⇒ getFunctionHelp(f)
    case bm: BoundMethod  ⇒ getMethodHelp(bm)
    case klass: MashClass ⇒ getClassHelp(klass)
    case value            ⇒ getClassHelp(value.primaryClass)
  }

  private def getFunctionHelp(f: MashFunction, classOpt: Option[MashClass] = None): MashObject = {
    import FunctionHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(f.name),
        FullyQualifiedName -> MashString(f.fullyQualifiedName.toString),
        Aliases -> MashList.empty,
        Summary -> f.summaryOpt.map(MashString(_)).getOrElse(MashNull),
        CallingSyntax -> MashString(f.name + " " + f.params.callingSyntax),
        Description -> f.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        Parameters -> MashList(f.params.params.map(getParamHelp)),
        Class -> classOpt.map(klass ⇒ MashString(klass.fullyQualifiedName.toString)).getOrElse(MashNull)),
      FunctionHelpClass)
  }

  private def getMethodHelp(boundMethod: BoundMethod): MashObject =
    getMethodHelp(boundMethod.method, boundMethod.klass)

  private def getMethodHelp(m: MashMethod, klass: MashClass): MashObject = {
    import FunctionHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(m.name),
        FullyQualifiedName -> MashString(m.name),
        Aliases -> MashList(m.aliases.map(MashString(_))),
        Summary -> m.summaryOpt.map(MashString(_)).getOrElse(MashNull),
        CallingSyntax -> MashString(m.name + " " + m.params.callingSyntax),
        Description -> m.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        Parameters -> MashList(m.params.params.map(getParamHelp)),
        Class -> MashString(klass.fullyQualifiedName.toString)),
      FunctionHelpClass)
  }

  def getFieldHelp(field: Field, klass: MashClass): MashObject = {
    import FieldHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(field.name),
        Class -> MashString(klass.fullyQualifiedName.toString),
        Summary -> field.summaryOpt.map(MashString(_)).getOrElse(MashNull),
        Description -> field.descriptionOpt.map(MashString(_)).getOrElse(MashNull)),
      FieldHelpClass)
  }

  private def getParamHelp(param: Parameter): MashObject = {
    import ParameterHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> getName(param),
        Summary -> param.summaryOpt.map(MashString(_)).getOrElse(MashNull),
        Description -> param.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        ShortFlag -> param.shortFlagOpt.map(c ⇒ MashString(c + "")).getOrElse(MashNull),
        IsFlagParameter -> MashBoolean(param.isFlag),
        IsOptional -> MashBoolean(param.hasDefault),
        IsLazy -> MashBoolean(param.isLazy),
        IsNamedArgs -> MashBoolean(param.isNamedArgsParam),
        IsVariadic -> MashBoolean(param.isVariadic)),
      ParameterHelpClass)
  }

  private def getName(param: Parameter): MashValue =
    param.nameOpt
      .filterNot(_ startsWith DesugarHoles.VariableNamePrefix)
      .map(MashString(_))
      .getOrElse(MashNull)

  private def getClassHelp(klass: MashClass): MashObject = {
    val fields = klass.fields.map(getFieldHelp(_, klass))
    val methods = klass.methods.filter(_.isPublic).sortBy(_.name).map(getMethodHelp(_, klass))
    val staticMethods = klass.staticMethods.map(getFunctionHelp(_, Some(klass)))
    val parent = klass.parentOpt.map(p ⇒ MashString(p.fullyQualifiedName.toString)).getOrElse(MashNull)
    val description = klass.descriptionOpt.map(MashString(_)).getOrElse(MashNull)
    val summary = klass.summaryOpt.map(MashString(_)).getOrElse(MashNull)
    val fullyQualifiedName = MashString(klass.fullyQualifiedName.toString)
    import ClassHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(klass.name),
        FullyQualifiedName -> fullyQualifiedName,
        Summary -> summary,
        Description -> description,
        Parent -> parent,
        Fields -> MashList(fields),
        Methods -> MashList(methods),
        StaticMethods -> MashList(staticMethods)),
      ClassHelpClass)
  }

}
