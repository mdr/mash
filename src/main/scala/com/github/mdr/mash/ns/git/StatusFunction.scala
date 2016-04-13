package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import java.time.Instant
import com.github.mdr.mash.os.linux.LinuxFileSystem
import org.eclipse.jgit.lib.Repository
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.functions.FunctionHelpers._
import org.eclipse.jgit.api.Status
import java.{ util ⇒ ju }

object StatusFunction extends MashFunction("git.status") {

  val params = ParameterModel()

  def apply(arguments: Arguments): MashObject = {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      val status = git.status.call()
      asMashObject(status)
    }
  }

  private def mashify(paths: ju.Set[String]): MashList = MashList(paths.asScala.toSeq.map(asPathString))

  private def asMashObject(status: Status): MashObject = {
    val modified = mashify(status.getModified)
    val untracked = mashify(status.getUntracked)
    val added = mashify(status.getAdded)
    val changed = mashify(status.getChanged)
    val removed = mashify(status.getRemoved)
    val missing = mashify(status.getMissing)

    import StatusClass.Fields._
    MashObject(ListMap(
      Added -> added,
      Changed -> changed,
      Missing -> missing,
      Modified -> modified,
      Removed -> removed,
      Untracked -> untracked), StatusClass)

  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(StatusClass))

  override def summary = "Return the working tree status"

}