package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass

object FunctionHelpClass extends MashClass("core.help.FunctionHelp") {

  object Fields {
    val Name = Field("name", "Function name", Type.Instance(StringClass))
    val FullyQualifiedName = Field("fullyQualifiedName", "Fully-qualified name of the function", Type.Instance(StringClass))
    val Summary = Field("summary", "Summary of what the function does", Type.Instance(StringClass))
    val CallingSyntax = Field("callingSyntax", "Calling syntax", Type.Instance(StringClass))
    val Description = Field("description", "Description of the function", Type.Instance(StringClass))
    val Parameters = Field("parameters", "Parameters of the function", Type.Seq(Type.Instance(ParameterHelpClass)))
    val Class = Field("class", "If a method, the class it belongs to (else null)", Type.Instance(StringClass))
  }

  import Fields._

  override val fields = Seq(Name, FullyQualifiedName, Summary, CallingSyntax, Description, Parameters, Class)

  override def summary = "Help documentation for a function"

}