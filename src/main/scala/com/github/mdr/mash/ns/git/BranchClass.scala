package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy

object BranchClass extends MashClass("git.Branch") {

  object Fields {
    val Name = Field("name", "Name of the branch", Type.Instance(StringClass))
    val Commit = Field("commit", "The commit the branch is pointing to", Type.Tagged(StringClass, CommitHashClass))
  }

  import Fields._

  override lazy val fields = Seq(Name, Commit)

  def summary = "A git branch"

  
  override lazy val methods = Seq(
    ToStringMethod)

  object ToStringMethod extends MashMethod("toString") {

    val params = ObjectClass.ToStringMethod.params

    def apply(target: Any, arguments: Arguments): MashString =
      target.asInstanceOf[MashObject].field(Fields.Name).asInstanceOf[MashString]

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(StringClass))

    override def summary = ObjectClass.ToStringMethod.summary

  }

}