package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }

import scala.collection.immutable.ListMap

object FieldHelpClass extends MashClass("core.help.FieldHelp") {

  object Fields {
    val Name = Field("name", Some("Field name"), StringClass)
    val Class = Field("class", Some("Class this field belongs to"), StringClass)
    val Summary = Field("summary", Some("Summary of what the function does"), StringClass)
    val Description = Field("description", Some("Description of the function"), StringClass)
  }

  import Fields._

  def create(name: String,
             klass: String,
             summaryOpt: Option[String],
             descriptionOpt: Option[String]): MashObject =
    MashObject.of(
      ListMap(
        Name -> MashString(name),
        Class -> MashString(klass),
        Summary -> summaryOpt.map(MashString(_)).getOrElse(MashNull),
        Description -> descriptionOpt.map(MashString(_)).getOrElse(MashNull)),
      FieldHelpClass)

  case class Wrapper(any: MashValue) extends AbstractObjectWrapper(any) {

    def name = getStringField(Name)

    def klass = getStringField(Class)

    def summaryOpt = getOptionalStringField(Summary)

    def descriptionOpt = getOptionalStringField(Description)

  }

  override val fields = Seq(Name, Class, Summary, Description)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Help documentation for a field")

}