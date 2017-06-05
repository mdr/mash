package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.ns.core.{ BooleanClass, StringClass }
import com.github.mdr.mash.runtime.MashValue

object ParameterHelpClass extends MashClass("core.help.ParameterHelp") {

  object Fields {
    val Name = Field("name", Some("Parameter name"), StringClass)
    val Summary = Field("summary", Some("Summary of what the parameter does"), StringClass)
    val Description = Field("description", Some("Description of the parameter"), StringClass)
    val ShortFlag = Field("shortFlag", Some("Alternative single-character flag form, or null if none"), StringClass)
    val IsFlagParameter = Field("isFlagParameter", Some("If true, this parameter can only be given as a flag"), BooleanClass)
    val IsOptional = Field("isOptional", Some("If true, this parameter is optional"), BooleanClass)
    val IsLazy = Field("isLazy", Some("If true, this parameter is evaluated lazily"), BooleanClass)
    val IsNamedArgs = Field("isNamedArg", Some("If true, this parameter is is a @namedArg parameter"), BooleanClass)
    val IsVariadic = Field("isVariadic", Some("If true, this parameter can take an arbitrary number of positional arguments"), BooleanClass)
  }

  import Fields._

  override val fields = Seq(Name, Summary, Description, ShortFlag, IsFlagParameter, IsOptional, IsLazy, IsNamedArgs, IsVariadic)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(any: MashValue) extends AbstractObjectWrapper(any) {

    def isFlag: Boolean = getBooleanField(IsFlagParameter)

    def isLazy: Boolean = getBooleanField(IsLazy)

    def isNamedArgs: Boolean = getBooleanField(IsNamedArgs)

    def isOptional: Boolean = getBooleanField(IsOptional)

    def isVariadic: Boolean = getBooleanField(IsVariadic)

    def nameOpt: Option[String] = getOptionalStringField(Name)

    def shortFlagOpt: Option[String] = getOptionalStringField(ShortFlag)

    def summaryOpt: Option[String] = getOptionalStringField(Summary)

    def descriptionOpt: Option[String] = getOptionalStringField(Description)

  }

  override def summaryOpt = Some("Help documentation for parameters")

}