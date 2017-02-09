package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.core.{ BooleanClass, StringClass }
import com.github.mdr.mash.ns.git.branch.{ CreateFunction, SwitchFunction }
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.diff.DiffEntry
import org.eclipse.jgit.lib.{ ObjectId, Repository }
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.CanonicalTreeParser

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object CommitClass extends MashClass("git.Commit") {

  object Fields {
    val Hash = Field("hash", Some("Commit hash"), StringClass taggedWith CommitHashClass)
    val CommitTime = Field("commitTime", Some("Commit time"), DateTimeClass)
    val Author = Field("author", Some("Author of the commit"), IdentityClass)
    val Committer = Field("committer", Some("Committer of the commit"), IdentityClass)
    val Summary = Field("summary", Some("Summary message of the commit"), StringClass)
    val Message = Field("message", Some("Commit message"), StringClass)
    val Parents = Field("parents", Some("Parents of this commit"), Seq(CommitHashClass))
  }
  import Fields._

  override lazy val fields = Seq(Hash, CommitTime, Author, Committer, Summary, Message, Parents)

  override lazy val methods = Seq(
    DiffMethod,
    IsAncestorOfMethod,
    ParentMethod,
    ToStringMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("A git commit object")

  case class Wrapper(target: MashValue) {

    def hash: MashString = target.asInstanceOf[MashObject](Hash).asInstanceOf[MashString]

    def parents: Seq[MashString] =
      target.asInstanceOf[MashObject](Parents).asInstanceOf[MashList].elements.map(_.asInstanceOf[MashString])

    def parentOpt: Option[MashString] = parents.headOption

  }

  object IsAncestorOfMethod extends AbstractIsAncestorOfMethod {

    override def aliases = Seq("isMergedInto)")

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

    override def typeInferenceStrategy = Type.Tagged(StringClass, CommitHashClass)

    override def summaryOpt = Some("Return the first parent, if there is one, else null")

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
      git.diff().setNewTree(childTreeIter).setOldTree(parentTreeIter).call().asScala
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

    override def typeInferenceStrategy = Seq(FileDiffClass)

    override def summaryOpt = Some("Return the difference between the first parent of this commit, and this commit")

  }

}

abstract class AbstractIsAncestorOfMethod extends MashMethod("isAncestorOf") {

  def commitName(target: MashValue): String

  object Params {
    val Commit = Parameter(
      nameOpt = Some("commit"),
      summaryOpt = Some("Name of a commit to test if it is descendant"))
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

  override def typeInferenceStrategy = BooleanClass

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }

  override def summaryOpt = Some("Return true if this is an ancestor of the given commit")

}