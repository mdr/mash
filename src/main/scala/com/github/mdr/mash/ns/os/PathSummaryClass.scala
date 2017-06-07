package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.pathClass.InfoMethod
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.os.PathSummary
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object PathSummaryClass extends MashClass("os.PathSummary") {

  object Fields {
    val Path = Field("path", Some("Path"), StringClass taggedWith PathClass)
    val Type = Field("type", Some("Type (file, dir, link)"), StringClass taggedWith FileTypeClass)
    val Size = Field("size", Some("Size in bytes"), NumberClass taggedWith BytesClass)
    val Owner = Field("owner", Some("User owner"), StringClass taggedWith UsernameClass)
    val Group = Field("group", Some("Group owner"), StringClass taggedWith GroupClass)
    val Permissions = Field("permissions", Some("Read/write/execute permissions"), PermissionsClass)
    val LastModified = Field("lastModified", Some("Last time path was modified"), DateTimeClass)
  }

  override lazy val fields = {
    import Fields._
    Seq(Path, Type, Size, Owner, Group, Permissions, LastModified)
  }

  override lazy val methods = {
    val omittedMethods = fields.map(_.name).toSet + InfoMethod.name
    PathClass.methods.filterNot(m ⇒ omittedMethods contains m.name).map(liftPathMethod)
  }

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    def path = getStringField(Fields.Path)

    def pathMashValue = getField(Fields.Path)
  }

  private def liftPathMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    override def call(target: MashValue, boundParams: BoundParams): MashValue =
      method.call(Wrapper(target).pathMashValue, boundParams)

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summaryOpt = method.summaryOpt

    override def descriptionOpt = method.descriptionOpt

  }

  def asMashObject(summary: PathSummary): MashObject = {
    val PathSummary(path, fileType, size, owner, group, permissions, lastModified, _) = summary
    MashObject.of(
      ListMap(
        Fields.Path -> asPathString(path),
        Fields.Type -> MashString(fileType, FileTypeClass),
        Fields.Size -> MashNumber(size, BytesClass),
        Fields.Owner -> MashString(owner, UsernameClass),
        Fields.Group -> MashString(group, GroupClass),
        Fields.Permissions -> PermissionsClass.asMashObject(permissions),
        Fields.LastModified -> MashWrapped(lastModified)),
      PathSummaryClass)
  }

  override def summaryOpt = Some("Information about a filesystem path")

}