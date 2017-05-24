package com.github.mdr.mash.functions

object Parameter {

  val AnonymousParamName = "anonymousParam"

}

case class Parameter(nameOpt: Option[String],
                     summaryOpt: Option[String] = None, // One-liner description of the parameter
                     descriptionOpt: Option[String] = None, // Rest of the docs
                     shortFlagOpt: Option[Char] = None, // Single character version of the flag
                     defaultValueGeneratorOpt: Option[ValueGenerator] = None, // Default value if none provided
                     isVariadic: Boolean = false, // Is a variadic parameter (can be bound to 0-to-many arguments)
                     variadicAtLeastOne: Boolean = false, // As a variadic parameter, must it have at least one argument?
                     variadicFlatten: Boolean = false, // Variadic arguments get flattened if they are lists
                     isFlag: Boolean = false, // If true, can only be called in flag mode, not positional
                     isBooleanFlag: Boolean = false, // If true, flag represents a boolean value (only affects calling syntax generation)
                     isFlagValueMandatory: Boolean = false, // If true, flag must have a value (only affects calling syntax generation)
                     flagValueNameOpt: Option[String] = None, // Name of flag value (used in generating calling summary)
                     isLast: Boolean = false, // If true, is the last parameter -- absorbs the last parameter in the list
                     isLazy: Boolean = false, // if true, don't evaluate argument
                     isNamedArgsParam: Boolean = false, // If true, receive a list of the named arguments
                     isAllArgsParam: Boolean = false, // If true, receive a list of all argument
                     patternOpt: Option[ParamPattern] = None // object pattern names to bind
                    ) {

  def isOptional = defaultValueGeneratorOpt.isDefined

  def isMandatory = !isOptional

  override def toString = name

  def boundNames: Seq[String] = nameOpt.map(Seq(_)).getOrElse(patternOpt.toSeq.flatMap(_.boundNames))

  def name = nameOpt getOrElse Parameter.AnonymousParamName
}

sealed trait ParamPattern {
  def boundNames: Seq[String] = Seq()
}

object ParamPattern {

  case class ObjectEntry(fieldName: String, patternOpt: Option[ParamPattern] = None)
  case class Object(entries: Seq[ObjectEntry]) extends ParamPattern {
    override def boundNames = entries.flatMap(_.patternOpt).flatMap(_.boundNames)
  }
  case class Ident(name: String) extends ParamPattern {
    override def boundNames = Seq(name)
  }
  case object Hole extends ParamPattern
  case class List(patterns: Seq[ParamPattern]) extends ParamPattern {
    override def boundNames = patterns.flatMap(_.boundNames)
  }

}