package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.AnyClass
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
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.ns.git.branch.SwitchFunction
import com.github.mdr.mash.ns.git.branch.CreateFunction
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.evaluator.AbstractToStringMethod

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
    IsAncestorOfMethod,
    ParentMethod,
    ToStringMethod)

  def summary = "A git commit object"

  case class Wrapper(target: MashValue) {

    def hash: MashString = target.asInstanceOf[MashObject](Hash).asInstanceOf[MashString]

    def parents: Seq[MashString] =
      target.asInstanceOf[MashObject](Parents).asInstanceOf[MashList].items.map(_.asInstanceOf[MashString])

    def parentOpt: Option[MashString] = parents.headOption

  }

  object IsAncestorOfMethod extends AbstractIsAncestorOfMethod {

    override def commitName(target: MashValue) = Wrapper(target).hash.s

  }

  object ToStringMethod extends AbstractToStringMethod {

    override def toString(target: MashValue) = Wrapper(target).hash.s

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      Wrapper(target).parentOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, CommitHashClass))

    override def summary = "Return the first parent, if there is one, else null"

  }

  object DiffMethod extends MashMethod("diff") {
    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments) = {
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
      def processPath(path: String): MashValue =
        Option(path).flatMap(handleDevNull).map(s ⇒ MashString(s, PathClass)).getOrElse(MashNull)
      MashObject.of(
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

abstract class AbstractIsAncestorOfMethod extends MashMethod("isAncestorOf") {

  def commitName(target: MashValue): String

  object Params {
    val Commit = Parameter(
      name = "commit",
      summary = "Name of a commit to test if it is descendant")
  }
  import Params._

  val params = ParameterModel(Seq(Commit))

  def apply(target: MashValue, arguments: Arguments): MashBoolean = {
    val boundParams = params.validate(arguments)
    val commit = MergeFunction.validateCommit(boundParams, Commit)

    GitHelper.withRepository { repo ⇒
      val revWalk = new RevWalk(repo)
      val thisCommit = revWalk.parseCommit(repo.resolve(commitName(target)))
      val thatCommit = revWalk.parseCommit(commit)
      MashBoolean(revWalk.isMergedInto(thisCommit, thatCommit))
    }
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }

  override def summary = "Return true if this is an ancestor of the given commit"

}