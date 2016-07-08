package com.github.mdr.mash.ns.os

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.parser.AbstractSyntax
import com.github.mdr.mash.os.PathSummary
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashWrapped
import com.github.mdr.mash.runtime.MashValue

object PathSummaryClass extends MashClass("os.PathSummary") {

  object Fields {
    import com.github.mdr.mash.inference.Type._
    val Path = Field("path", "Path", Tagged(StringClass, PathClass))
    val Type = Field("type", "Type (file, dir, link)", Tagged(StringClass, FileTypeClass))
    val Size = Field("size", "Size in bytes", Tagged(NumberClass, BytesClass))
    val Owner = Field("owner", "User owner", Tagged(StringClass, UsernameClass))
    val Group = Field("group", "Group owner", Tagged(StringClass, GroupClass))
    val Permissions = Field("permissions", "Read/write/execute permissions", Instance(PermissionsClass))
    val LastModified = Field("lastModified", "Last time path was modified", Instance(DateTimeClass))
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

  private case class Wrapper(obj: MashValue) {
    def path: MashString = obj.asInstanceOf[MashObject](Fields.Path).asInstanceOf[MashString]
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