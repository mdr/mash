package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.ns.core.{ BooleanClass, StringClass }

object ParameterHelpClass extends MashClass("core.help.ParameterHelp") {

  object Fields {
    val Name = Field("name", "Parameter name", StringClass)
    val Summary = Field("summary", "Summary of what the parameter does", StringClass)
    val Description = Field("description", "Description of the parameter", StringClass)
    val ShortFlag = Field("shortFlag", "Alternative single-character flag form, or null if none", StringClass)
    val IsFlagParameter = Field("isFlagParameter", "If true, this parameter can only be given as a flag", BooleanClass)
    val IsOptional = Field("isOptional", "If true, this parameter is optional", BooleanClass)
    val IsLast = Field("isLast", "If true, this parameter is the last positonal parameter", BooleanClass)
    val IsLazy = Field("isLazy", "If true, this parameter is evaluated lazily", BooleanClass)
    val IsVariadic = Field("isVariadic", "If true, this parameter can take an arbitrary number of positional arguments", BooleanClass)
  }

  import Fields._

  override val fields = Seq(Name, Summary, Description, ShortFlag, IsFlagParameter, IsOptional, IsLast, IsLazy, IsVariadic)

  override def summary = "Help documentation for parameters"

}