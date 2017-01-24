package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.pathClass.PathClassInfoMethod
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
    val omittedMethods = fields.map(_.name).toSet + PathClassInfoMethod.name
    val liftedMethods = PathClass.methods.filterNot(m ⇒ omittedMethods contains m.name).map(liftPathMethod)
    ToStringMethod +: liftedMethods
  }

  private case class Wrapper(value: MashValue) {
    private val obj = value.asInstanceOf[MashObject]
    
    def path: MashString = obj.fieldAs[MashString](Fields.Path)
  }

  private def liftPathMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    def apply(target: MashValue, arguments: Arguments): MashValue =
      method.apply(Wrapper(target).path, arguments)

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summary = method.summary

    override def descriptionOpt = method.descriptionOpt

  }

  object ToStringMethod extends AbstractToStringMethod {

    override def toString(target: MashValue) = Wrapper(target).path.s

  }

  def asMashObject(summary: PathSummary): MashObject = {
    val PathSummary(path, fileType, size, owner, group, permissions, lastModified, lastAccessed) = summary
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

  override def summary = "Information about a filesystem path"

}