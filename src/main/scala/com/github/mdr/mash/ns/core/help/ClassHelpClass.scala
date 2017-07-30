package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.ns.core.{ ClassClass, StringClass }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

import scala.collection.immutable.ListMap

object ClassHelpClass extends MashClass("core.help.ClassHelp") {

  object Fields {
    val Name = Field("name", Some("Function name"), StringClass)
    val FullyQualifiedName = Field("fullyQualifiedName", Some("Fully-qualified name of the class"), StringClass)
    val Summary = Field("summary", Some("Summary of what the class does"), StringClass)
    val Description = Field("description", Some("Description of the class"), StringClass)
    val Parent = Field("parent", Some("The parent class, if any, else null"), ClassClass)
    val Fields = Field("fields", Some("The fields of the class"), Seq(FieldHelpClass))
    val Methods = Field("methods", Some("The methods of the class"), Seq(FunctionHelpClass))
    val StaticMethods = Field("staticMethods", Some("The static methods of the class"), Seq(FunctionHelpClass))
  }

  import Fields._

  override val fields = Seq(Name, FullyQualifiedName, Summary, Description, Parent, Fields.Fields, Methods, StaticMethods)

  override val staticMethods = Seq(NewStaticMethod(this))

  def create(name: String,
             fullyQualifiedName: String,
             summaryOpt: Option[String],
             descriptionOpt: Option[String],
             parentOpt: Option[String],
             fields: Seq[MashObject],
             methods: Seq[MashObject],
             staticMethods: Seq[MashObject]): MashObject =
    MashObject.of(
      ListMap(
        Name -> MashString(name),
        FullyQualifiedName -> MashString(fullyQualifiedName),
        Summary -> MashString.maybe(summaryOpt),
        Description -> MashString.maybe(descriptionOpt),
        Parent -> MashString.maybe(parentOpt),
        Fields.Fields -> MashList(fields),
        Methods -> MashList(methods),
        StaticMethods -> MashList(staticMethods)),
      ClassHelpClass)

  case class Wrapper(any: MashValue) extends AbstractObjectWrapper(any) {

    def parentOpt: Option[String] = getOptionalStringField(Parent)

    def summaryOpt: Option[String] = getOptionalStringField(Summary)

    def descriptionOpt: Option[String] = getOptionalStringField(Description)

    def fullyQualifiedName: String = getStringField(FullyQualifiedName)

    def fields: Seq[FieldHelpClass.Wrapper] = getListField(Fields.Fields).map(FieldHelpClass.Wrapper)

    def staticMethods: Seq[FunctionHelpClass.Wrapper] = getListField(StaticMethods).map(FunctionHelpClass.Wrapper)

    def methods: Seq[MethodHelpClass.Wrapper] = getListField(Methods).map(MethodHelpClass.Wrapper)

  }

  override def summaryOpt = Some("Help documentation for a class")

}