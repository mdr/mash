package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.ClassClass
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt
import scala.collection.immutable.ListMap

object HelpFunction extends MashFunction("core.help.help") {

  object Params {
    val Item = Parameter(
      name = "item",
      summary = "The item to find help for")
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val item = boundParams(Item)
    getHelp(item).getOrElse(
      boundParams.throwInvalidArgument(Item, "No help available for value of type " + item.typeName))
  }

  override def typeInferenceStrategy = HelpTypeInferenceStrategy

  override def summary = "Find help for the given function, method, field or class"

  def getHelp(item: MashValue): Option[MashObject] = condOpt(item) {
    case f: MashFunction  ⇒ getHelp(f)
    case bm: BoundMethod  ⇒ getHelp(bm)
    case klass: MashClass ⇒ getHelp(klass)
  }

  def getHelp(f: MashFunction): MashObject = {
    import FunctionHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(f.name),
        FullyQualifiedName -> MashString(f.fullyQualifiedName.toString),
        Summary -> MashString(f.summary),
        CallingSyntax -> MashString(f.name + " " + f.params.callingSyntax),
        Description -> f.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        Parameters -> MashList(f.params.params.map(getHelp)),
        Class -> MashNull),
      FunctionHelpClass)
  }

  def getHelp(boundMethod: BoundMethod): MashObject =
    getHelp(boundMethod.method, boundMethod.klass)

  def getHelp(m: MashMethod, klass: MashClass): MashObject = {
    import FunctionHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(m.name),
        FullyQualifiedName -> MashString(m.name),
        Summary -> MashString(m.summary),
        CallingSyntax -> MashString(m.name + " " + m.params.callingSyntax),
        Description -> m.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        Parameters -> MashList(m.params.params.map(getHelp)),
        Class -> MashString(klass.fullyQualifiedName.toString)),
      FunctionHelpClass)
  }

  def getHelp(field: Field, klass: MashClass): MashObject = {
    import FieldHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(field.name),
        Class -> MashString(klass.fullyQualifiedName.toString),
        Summary -> MashString(field.summary),
        Description -> field.descriptionOpt.map(MashString(_)).getOrElse(MashNull)),
      FieldHelpClass)
  }

  def getHelp(param: Parameter): MashObject = {
    import ParameterHelpClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(param.name),
        Summary -> MashString(param.summary),
        Description -> param.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        ShortFlag -> param.shortFlagOpt.map(c ⇒ MashString(c + "")).getOrElse(MashNull),
        IsFlagParameter -> MashBoolean(param.isFlag),
        IsOptional -> MashBoolean(param.isOptional),
        IsLast -> MashBoolean(param.isLast),
        IsLazy -> MashBoolean(param.isLazy),
        IsVariadic -> MashBoolean(param.isVariadic)),
      ParameterHelpClass)
  }

  def getHelp(klass: MashClass): MashObject = {
    import ClassHelpClass.Fields._
    val fields = klass.fields.map(getHelp(_, klass))
    val methods = klass.methods.sortBy(_.name).map(getHelp(_, klass))
    MashObject.of(
      ListMap(
        Name -> MashString(klass.name),
        FullyQualifiedName -> MashString(klass.fullyQualifiedName.toString),
        Summary -> MashString(klass.summary),
        Description -> klass.descriptionOpt.map(MashString(_)).getOrElse(MashNull),
        Parent -> klass.parentOpt.map(p ⇒ MashString(p.fullyQualifiedName.toString)).getOrElse(MashNull),
        Fields -> MashList(fields),
        Methods -> MashList(methods)),
      ClassHelpClass)
  }

}

object HelpTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = HelpFunction.params.bindTypes(arguments)
    import HelpFunction.Params._
    argBindings.getType(Item).collect {
      case Type.DefinedFunction(_) | Type.Lambda(_, _, _) | Type.BoundMethod(_, _) ⇒ FunctionHelpClass
      case Type.Instance(ClassClass)                                               ⇒ ClassHelpClass
    }

  }

}