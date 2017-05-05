package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.lib.ReflogEntry

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object ReflogFunction extends MashFunction("git.reflog") {

  override def params: ParameterModel = ParameterModel()

  override def call(boundParams: BoundParams): MashList =
    GitHelper.withGit { git ⇒
      val refLogEntries = git.reflog.call().asScala.toSeq.reverse
      MashList(refLogEntries.map(asMashObject))
    }

  private def asMashObject(entry: ReflogEntry): MashObject = {
    import com.github.mdr.mash.ns.git.ReflogEntryClass.Fields._
    MashObject.of(
      ListMap(
        Commit -> MashString(entry.getNewId.getName),
        Comment -> MashString(entry.getComment)),
      ReflogEntryClass)
  }

  override def summaryOpt: Option[String] = Some("The Git reference log")

}
