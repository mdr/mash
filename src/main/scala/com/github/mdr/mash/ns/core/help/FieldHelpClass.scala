package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }

object FieldHelpClass extends MashClass("core.help.FieldHelp") {

  object Fields {
    val Name = Field("name", Some("Field name"), StringClass)
    val Class = Field("class", Some("Class this field belongs to"), StringClass)
    val Summary = Field("summary", Some("Summary of what the function does"), StringClass)
    val Description = Field("description", Some("Description of the function"), StringClass)
  }

  import Fields._

  case class Wrapper(any: MashValue) {
    val obj = any.asInstanceOf[MashObject]

    def name = obj(Name).asInstanceOf[MashString].s

    def klass = obj(Class).asInstanceOf[MashString].s

    def summaryOpt = MashNull.option(obj(Summary)).map(_.asInstanceOf[MashString].s)

    def descriptionOpt = MashNull.option(obj(Description)).map(_.asInstanceOf[MashString].s)
  }

  override val fields = Seq(Name, Class, Summary, Description)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Help documentation for a field")

}