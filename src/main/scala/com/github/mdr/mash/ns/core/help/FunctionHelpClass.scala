package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.ns.core.StringClass

object FunctionHelpClass extends MashClass("core.help.FunctionHelp") {

  object Fields {
    val Name = Field("name", Some("Function name"), StringClass)
    val FullyQualifiedName = Field("fullyQualifiedName", Some("Fully-qualified name of the function"), StringClass)
    val Summary = Field("summary", Some("Summary of what the function does"), StringClass)
    val CallingSyntax = Field("callingSyntax", Some("Calling syntax"), StringClass)
    val Description = Field("description", Some("Description of the function"), StringClass)
    val Parameters = Field("parameters", Some("Parameters of the function"), Seq(ParameterHelpClass))
    val Class = Field("class", Some("If a method, the class it belongs to (else null)"), StringClass)
  }

  import Fields._

  override val fields = Seq(Name, FullyQualifiedName, Summary, CallingSyntax, Description, Parameters, Class)

  override def summaryOpt = Some("Help documentation for a function")

}