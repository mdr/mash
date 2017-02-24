package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, FullyQualifiedName, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.git.{ CommitHashClass, GitHelper }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString }
import org.eclipse.jgit.lib.Ref

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object ListFunction extends MashFunction("git.tag.list") {

  override def aliases = Seq(FullyQualifiedName("git.tags"))

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashList = {
    GitHelper.withGit { git ⇒
      val tags = git.tagList.call().asScala
      MashList(tags.map(asMashObject))
    }
  }

  def getTagNames: Seq[String] =
    try
      GitHelper.withGit(_.tagList.call().asScala.map(getTagName))
    catch {
      case _: Exception ⇒ Seq()
    }

  def asMashObject(ref: Ref): MashObject = {
    val id = ref.getObjectId.getName
    val name = getTagName(ref)
    import TagClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(name, TagNameClass),
        Commit -> MashString(id, CommitHashClass)),
      TagClass)
  }

  private def getTagName(ref: Ref): String = ref.getName.replaceAll("^refs/tags/", "")

  override def typeInferenceStrategy = Seq(TagClass)

  override def summaryOpt = Some("Get a list of tags")
}
