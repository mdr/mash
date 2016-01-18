package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.ns.core.StringClass

object ParameterHelpClass extends MashClass("core.help.ParameterHelp") {

  object Fields {
    val Name = Field("name", "Parameter name", Type.Instance(StringClass))
    val Summary = Field("summary", "Summary of what the parameter does", Type.Instance(StringClass))
    val Description = Field("description", "Description of the parameter", Type.Instance(StringClass))
    val ShortFlag = Field("shortFlag", "Alternative single-character flag form, or null if none", Type.Instance(StringClass))
    val IsFlagParameter = Field("isFlageter", "If true, this parameter can only be given as a flag", Type.Instance(BooleanClass))
    val IsOptional = Field("isOptional", "If true, this parameter is optional", Type.Instance(BooleanClass))
    val IsLast = Field("isLast", "If true, this parameter is the last positonal parameter", Type.Instance(BooleanClass))
    val IsVariadic = Field("isVariadic", "If true, this parameter can take an arbitrary number of positional arguments", Type.Instance(BooleanClass))
  }

  import Fields._

  override val fields = Seq(Name, Summary, Description, ShortFlag, IsFlagParameter, IsOptional, IsLast, IsVariadic)

  override def summary = "Help documentation for parameters"

}