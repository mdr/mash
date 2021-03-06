package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.{ ClassClass, StringClass }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object MethodHelpClass extends MashClass("core.help.MethodHelp") {

  object Fields {
    val Name = Field("name", Some("Method name"), StringClass)
    val OwningClass = Field("owningClass", Some("The class the method belongs to"), ClassClass)
  }

  import Fields._

  override val fields = Seq(Name, OwningClass)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Help documentation for a method")

  def create(name: String,
             owningClass: MashClass): MashObject =
    MashObject.of(
      ListMap(
        Name -> MashString(name),
        OwningClass -> owningClass),
      MethodHelpClass)

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    def name: String = getStringField(Name)

    def klass: MashClass = getClassField(OwningClass)

    def method: MashMethod = klass.getMethod(name).getOrElse(
      throw EvaluatorException(s"No method '$name' found in '${klass.fullyQualifiedName}'"))

  }

}
