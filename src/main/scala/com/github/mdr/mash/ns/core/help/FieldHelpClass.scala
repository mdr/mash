package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.ns.core.{ ClassClass, StringClass }
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }

import scala.collection.immutable.ListMap

object FieldHelpClass extends MashClass("core.help.FieldHelp") {

  object Fields {
    val Name = Field("name", Some("Field name"), StringClass)
    val OwningClass = Field("owningClass", Some("Class this field belongs to"), ClassClass)
  }

  import Fields._

  def create(name: String,
             owningClass: MashClass): MashObject =
    MashObject.of(
      ListMap(
        Name -> MashString(name),
        OwningClass -> owningClass),
      FieldHelpClass)

  case class Wrapper(any: MashValue) extends AbstractObjectWrapper(any) {

    def name = getStringField(Name)

    def klass = getClassField(OwningClass)

    def field = klass.getField(name).getOrElse(
      throw new EvaluatorException(s"No field '$name' found in '${klass.fullyQualifiedName}'"))

  }

  override val fields = Seq(Name, OwningClass)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Help documentation for a field")

}