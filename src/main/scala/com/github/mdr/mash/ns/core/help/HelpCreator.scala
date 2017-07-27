package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ BoundMethod, Field, MashClass, UserDefinedMethod }
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, UserDefinedFunction }
import com.github.mdr.mash.runtime._

object HelpCreator {

  def getHelp(item: MashValue): MashObject = item match {
    case f: MashFunction  ⇒ getFunctionHelp(f)
    case bm: BoundMethod  ⇒ getMethodHelp(bm)
    case klass: MashClass ⇒ getClassHelp(klass)
    case value            ⇒ getClassHelp(value.primaryClass)
  }

  private def getFunctionHelp(f: MashFunction, classOpt: Option[MashClass] = None): MashObject =
    FunctionHelpClass.create(
      name = f.name,
      fullyQualifiedName = f.fullyQualifiedName.toString,
      aliases = Seq(),
      summaryOpt = f.summaryOpt,
      callingSyntax = f.name + (if (f.params.isEmpty) "" else " " + f.params.callingSyntax),
      descriptionOpt = f.descriptionOpt,
      parameters = f.params.params.map(getParamHelp),
      classOpt = classOpt.map(_.fullyQualifiedName.toString),
      sourceOpt = getSource(f),
      functionOpt = Some(f))

  private def getSource(f: MashFunction): Option[String] = f match {
    case udf: UserDefinedFunction ⇒ udf.sourceLocationOpt.map(_.source)
    case _                        ⇒ None
  }

  private def getSource(m: MashMethod): Option[String] = m match {
    case udf: UserDefinedMethod ⇒ udf.sourceLocationOpt.map(_.source)
    case _                      ⇒ None
  }

  private def getMethodHelp(boundMethod: BoundMethod): MashObject =
    getMethodHelp(boundMethod.method, boundMethod.klass, Some(boundMethod))

  private def getMethodHelp(m: MashMethod, klass: MashClass, boundMethodOpt: Option[BoundMethod]): MashObject =
    FunctionHelpClass.create(
      name = m.name,
      fullyQualifiedName = m.name,
      aliases = m.aliases,
      summaryOpt = m.summaryOpt,
      callingSyntax = m.name + (if (m.params.isEmpty) "" else " " + m.params.callingSyntax),
      descriptionOpt = m.descriptionOpt,
      parameters = m.params.params.map(getParamHelp),
      classOpt = Some(klass.fullyQualifiedName.toString),
      sourceOpt = getSource(m),
      functionOpt = boundMethodOpt)

  def getFieldHelp(field: Field, klass: MashClass): MashObject =
    FieldHelpClass.create(
      name = field.name,
      klass = klass.fullyQualifiedName.toString,
      summaryOpt = field.summaryOpt,
      descriptionOpt = field.descriptionOpt)

  private def getParamHelp(param: Parameter): MashObject =
    ParameterHelpClass.create(
      nameOpt = param.nameOpt.filterNot(_ startsWith DesugarHoles.VariableNamePrefix),
      summaryOpt = param.summaryOpt,
      descriptionOpt = param.descriptionOpt,
      shortFlagOpt = param.shortFlagOpt,
      isFlag = param.isFlag,
      isOptional = param.hasDefault,
      isLazy = param.isLazy,
      isNamedArgs = param.isNamedArgsParam,
      isVariadic = param.isVariadic)

  private def getClassHelp(klass: MashClass): MashObject =
    ClassHelpClass.create(
      name = klass.name,
      fullyQualifiedName = klass.fullyQualifiedName.toString,
      summaryOpt = klass.summaryOpt,
      descriptionOpt = klass.descriptionOpt,
      parentOpt = klass.parentOpt.map(_.fullyQualifiedName.toString),
      fields = klass.fields.map(getFieldHelp(_, klass)),
      methods = klass.methods.filter(_.isPublic).sortBy(_.name).map(getMethodHelp(_, klass, None)),
      staticMethods = klass.staticMethods.map(getFunctionHelp(_, Some(klass))))

}