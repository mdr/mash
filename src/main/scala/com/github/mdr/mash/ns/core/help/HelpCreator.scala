package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ BoundMethod, Field, MashClass }
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, UserDefinedFunction }
import com.github.mdr.mash.runtime._

object HelpCreator {

  def getHelp(item: MashValue): MashValue = item match {
    case f: MashFunction  ⇒ f
    case bm: BoundMethod  ⇒ getMethodHelp(bm)
    case klass: MashClass ⇒ getClassHelp(klass)
    case value            ⇒ getClassHelp(value.primaryClass)
  }

  private def getMethodHelp(boundMethod: BoundMethod): MashObject =
    getMethodHelp(boundMethod.method, boundMethod.klass)

  private def getMethodHelp(m: MashMethod, klass: MashClass): MashObject =
    MethodHelpClass.create(m.name, klass)

  def getFieldHelp(field: Field, klass: MashClass): MashObject =
    FieldHelpClass.create(
      name = field.name,
      klass = klass.fullyQualifiedName.toString,
      summaryOpt = field.summaryOpt,
      descriptionOpt = field.descriptionOpt)

  def getParamHelp(param: Parameter): MashObject =
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
      methods = klass.methods.filter(_.isPublic).sortBy(_.name).map(getMethodHelp(_, klass)),
      staticMethods = klass.staticMethods)

}