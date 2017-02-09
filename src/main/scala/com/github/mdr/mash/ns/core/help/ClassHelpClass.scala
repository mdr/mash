package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.{ ClassClass, StringClass }

object ClassHelpClass extends MashClass("core.help.ClassHelp") {

  object Fields {
    val Name = Field("name", Some("Function name"), StringClass)
    val FullyQualifiedName = Field("fullyQualifiedName", Some("Fully-qualified name of the class"), StringClass)
    val Summary = Field("summary", Some("Summary of what the class does"), StringClass)
    val Description = Field("description", Some("Description of the class"), StringClass)
    val Parent = Field("parent", Some("The parent class, if any, else null"), ClassClass)
    val Fields = Field("fields", Some("The fields of the class"), Type.Seq(FieldHelpClass))
    val Methods = Field("methods", Some("The methods of the class"), Type.Seq(FunctionHelpClass))
  }

  import Fields._

  override val fields = Seq(Name, FullyQualifiedName, Summary, Description, Parent, Fields.Fields, Methods)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Help documentation for a class")

}