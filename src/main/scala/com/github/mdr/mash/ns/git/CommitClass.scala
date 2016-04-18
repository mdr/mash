package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.time.DateTimeClass
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.api.Git
import scala.collection.JavaConverters._
import org.eclipse.jgit.revwalk.RevWalk
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.os.PathClass
import org.eclipse.jgit.diff.DiffEntry
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.lib.ObjectId

object CommitClass extends MashClass("git.Commit") {

  object Fields {
    val Hash = Field("hash", "Commit hash", Type.Tagged(StringClass, CommitHashClass))
    val CommitTime = Field("commitTime", "Commit time", DateTimeClass)
    val Author = Field("author", "Author of the commit", IdentityClass)
    val Committer = Field("committer", "Committer of the commit", IdentityClass)
    val Summary = Field("summary", "Summary message of the commit", StringClass)
    val Message = Field("message", "Commit message", StringClass)
    val Parents = Field("parents", "Parents of this commit", Seq(CommitHashClass))
  }
  import Fields._

  override lazy val fields = Seq(Hash, CommitTime, Author, Committer, Summary, Message, Parents)

  override lazy val methods = Seq(
    DiffMethod,
    ParentMethod,
    ToStringMethod)

  def summary = "A git commit object"

  case class Wrapper(target: Any) {

    def hash: MashString = target.asInstanceOf[MashObject].field(Hash).asInstanceOf[MashString]

    def parents: Seq[MashString] =
      target.asInstanceOf[MashObject].field(Parents).asInstanceOf[MashList].items.map(_.asInstanceOf[MashString])

    def parentOpt: Option[MashString] = parents.headOption

  }

  object ToStringMethod extends MashMethod("toString") {

    val params = ObjectClass.ToStringMethod.params

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).hash
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, CommitHashClass))

    override def summary = ObjectClass.ToStringMethod.summary

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).parentOpt.orNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, CommitHashClass))

    override def summary = "Return the first parent, if there is one, else null"

  }

  object DiffMethod extends MashMethod("diff") {
    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments) = {
      params.validate(arguments)
      val wrapper = Wrapper(target)
      val parentSha = wrapper.parentOpt.get.s
      val childSha = wrapper.hash.s

      GitHelper.withRepository { repo ⇒
        val parentId = getTreeId(repo, parentSha)
        val childId = getTreeId(repo, childSha)
        val diffs = getDiffs(repo, parentId, childId)
        MashList(diffs.map(asMashObject))
      }
    }

    private def getDiffs(repo: Repository, parentId: ObjectId, childId: ObjectId): Seq[DiffEntry] = {
      val reader = repo.newObjectReader
      val parentTreeIter = new CanonicalTreeParser
      parentTreeIter.reset(reader, parentId)
      val childTreeIter = new CanonicalTreeParser
      childTreeIter.reset(reader, childId)
      val git = new Git(repo)
      git.diff().setNewTree(childTreeIter).setOldTree(parentTreeIter).call().asScala.toSeq
    }

    private def getTreeId(repo: Repository, commitSha: String): ObjectId = {
      val treeId = repo.resolve(commitSha)
      val walk = new RevWalk(repo)
      val commit = walk.parseCommit(treeId)
      commit.getTree.getId
    }

    private def asMashObject(diff: DiffEntry): MashObject = {
      import FileDiffClass.Fields._
      def handleDevNull(s: String): Option[String] = s match {
        case "/dev/null" ⇒ None
        case _           ⇒ Some(s)
      }
      def processPath(path: String): MashString =
        Option(path).flatMap(handleDevNull).map(s ⇒ MashString(s, PathClass)).orNull
      MashObject(
        ListMap(
          _Type -> MashString(diff.getChangeType.name.toLowerCase.capitalize),
          OldPath -> processPath(diff.getOldPath),
          NewPath -> processPath(diff.getNewPath)),
        FileDiffClass)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(FileDiffClass))

    override def summary = "Return the difference between the first parent of this commit, and this commit"

  }

}