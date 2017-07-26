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

  private def getFunctionHelp(f: MashFunction, classOpt: Option[MashClass] = None): MashObject =
    FunctionHelpClass.create(
      name = f.name,
      fullyQualifiedName = f.fullyQualifiedName.toString,
      aliases = Seq(),
      summaryOpt = f.summaryOpt,
      callingSyntax = f.name + " " + f.params.callingSyntax,
      descriptionOpt = f.descriptionOpt,
      parameters = f.params.params.map(getParamHelp),
      classOpt = classOpt.map(_.fullyQualifiedName.toString))

  private def getMethodHelp(boundMethod: BoundMethod): MashObject =
    getMethodHelp(boundMethod.method, boundMethod.klass)

  private def getMethodHelp(m: MashMethod, klass: MashClass): MashObject =
    FunctionHelpClass.create(
      name = m.name,
      fullyQualifiedName = m.name,
      aliases = m.aliases,
      summaryOpt = m.summaryOpt,
      callingSyntax = m.name + " " + m.params.callingSyntax,
      descriptionOpt = m.descriptionOpt,
      parameters = m.params.params.map(getParamHelp),
      classOpt = Some(klass.fullyQualifiedName.toString))

  def getFieldHelp(field: Field, klass: MashClass): MashObject =
    FieldHelpClass.create(field.name, klass.fullyQualifiedName.toString, field.summaryOpt, field.descriptionOpt)

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

  private def getClassHelp(klass: MashClass): MashObject = {
    val fields = klass.fields.map(getFieldHelp(_, klass))
    val methods = klass.methods.filter(_.isPublic).sortBy(_.name).map(getMethodHelp(_, klass))
    val staticMethods = klass.staticMethods.map(getFunctionHelp(_, Some(klass)))
    val parent = klass.parentOpt.map(p ⇒ MashString(p.fullyQualifiedName.toString)).getOrElse(MashNull)
    val description = MashString.maybe(klass.descriptionOpt)
    val summary = MashString.maybe(klass.summaryOpt)
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
