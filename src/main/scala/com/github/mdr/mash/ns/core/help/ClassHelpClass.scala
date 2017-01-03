package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.{ ClassClass, StringClass }

object ClassHelpClass extends MashClass("core.help.ClassHelp") {

  object Fields {
    val Name = Field("name", "Function name", StringClass)
    val FullyQualifiedName = Field("fullyQualifiedName", "Fully-qualified name of the class", StringClass)
    val Summary = Field("summary", "Summary of what the class does", StringClass)
    val Description = Field("description", "Description of the class", StringClass)
    val Parent = Field("parent", "The parent class, if any, else null", ClassClass)
    val Fields = Field("fields", "The fields of the class", Type.Seq(FieldHelpClass))
    val Methods = Field("methods", "The methods of the class", Type.Seq(FunctionHelpClass))
  }

  import Fields._

  override val fields = Seq(Name, FullyQualifiedName, Summary, Description, Parent, Fields.Fields, Methods)

  override def summary = "Help documentation for a class"

}