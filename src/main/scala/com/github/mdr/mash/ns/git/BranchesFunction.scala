package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.UnitClass
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator.MashObject
import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator.MashString
import org.eclipse.jgit.lib.BranchConfig
import org.eclipse.jgit.lib.Ref

object BranchesFunction extends MashFunction("git.branches") {

  val params = ParameterModel(Seq())

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      val branches = git.branchList.setListMode(ListMode.ALL).call().asScala
      MashList(branches.map(asMashObject))
    }
  }

  private def asMashObject(ref: Ref): MashObject = {
    val name = ref.getName
    val id = ref.getObjectId.getName
    import BranchClass.Fields._
    MashObject(
      ListMap(
        Name -> MashString(name),
        Commit -> MashString(id, CommitHashClass)),
      BranchClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(BranchClass)))

  override def summary = "List branches in the repository"

}