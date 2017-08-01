package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass }
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ BooleanClass, NumberClass, StringClass }
import com.github.mdr.mash.ns.git.branch.LocalBranchNameClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime._

object StatusClass extends MashClass("git.Status") {

  object Fields {
    val Branch = Field("branch", Some("Current branch"), StringClass taggedWith LocalBranchNameClass)
    val UpstreamBranch = Field("upstreamBranch", Some("The name of the upstream branch (if any, else null)"), StringClass)
    val AheadCount = Field("aheadCount", Some("Number of commits that the local branch is ahead of the remote-tracking branch"), NumberClass)
    val BehindCount = Field("behindCount", Some("Number of commits that the local branch is behind the remote-tracking branch"), NumberClass)
    val Added = Field("added", Some("New files that have been staged"), Seq(PathClass))
    val Changed = Field("changed", Some("Changed files that have been staged"), Seq(PathClass))
    val Missing = Field("missing", Some("Files that have been deleted, but not staged"), Seq(PathClass))
    val Modified = Field("modified", Some("Modified files that have not been staged"), Seq(PathClass))
    val Removed = Field("removed", Some("Files that have been deleted and staged"), Seq(PathClass))
    val Untracked = Field("untracked", Some("Untracked files"), Seq(PathClass))
    val Conflicting = Field("conflicting", Some("Conflicting files"), Seq(PathClass))
  }

  import Fields._

  override lazy val fields =
    Seq(Branch, UpstreamBranch, AheadCount, BehindCount, Added, Changed, Missing, Modified, Removed, Untracked, Conflicting)

  override lazy val methods = Seq(
    IsCleanMethod)

  object IsCleanMethod extends MashMethod("isClean") {
    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
      val wrapper = Wrapper(target)
      val dirty = wrapper.hasChangesToBeCommitted || wrapper.hasUnstagedChanges || wrapper.untracked.nonEmpty ||
        wrapper.conflicting.nonEmpty
      MashBoolean(!dirty)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Return true if and only if the repository is in a clean state (no modified or untracked files)")

  }

  override def summaryOpt = Some("The status of a git repository")

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    private def unmashify(field: Field): Seq[String] =
      getListField(field).map(_.asInstanceOf[MashString].s)

    def added = unmashify(Added)

    def changed = unmashify(Changed)

    def missing = unmashify(Missing)

    def modified = unmashify(Modified)

    def removed = unmashify(Removed)

    def untracked = unmashify(Untracked)

    def conflicting = unmashify(Conflicting)

    def hasChangesToBeCommitted = added.nonEmpty || changed.nonEmpty || removed.nonEmpty

    def hasUnstagedChanges = modified.nonEmpty || missing.nonEmpty

    def branch = getStringField(Branch)

    def upstreamBranchOpt: Option[String] = getOptionalStringField(UpstreamBranch)

    def aheadCount = getNumberField(AheadCount).toInt

    def behindCount = getNumberField(BehindCount).toInt
  }

}