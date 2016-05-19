package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.UnitClass

object BranchClass extends MashClass("git.Branch") {

  object Fields {
    val Name = Field("name", "Name of the branch", Type.Tagged(StringClass, LocalBranchNameClass))
    val Commit = Field("commit", "The commit the branch is pointing to", Type.Tagged(StringClass, CommitHashClass))
    val UpstreamBranch = Field("upstreamBranch", "The upstream branch this branch is tracking, if any, else null", Type.Instance(StringClass))
  }

  import Fields._

  override lazy val fields = Seq(Name, Commit, UpstreamBranch)

  def summary = "A git branch"

  override lazy val methods = Seq(
    SwitchMethod,
    ToStringMethod)

  private case class Wrapper(target: Any) {

    def name = target.asInstanceOf[MashObject].field(Fields.Name).asInstanceOf[MashString]

  }

  object SwitchMethod extends MashMethod("switch") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments) {
      params.validate(arguments)
      val branchName = Wrapper(target).name.s
      GitHelper.withGit { git ⇒
        git.checkout().setName(branchName).call()
      }
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(UnitClass)

    override def summary = """Switch to this branch"""

  }

  object ToStringMethod extends MashMethod("toString") {

    val params = ObjectClass.ToStringMethod.params

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).name
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(StringClass))

    override def summary = ObjectClass.ToStringMethod.summary

  }

}