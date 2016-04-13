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

object PushFunction extends MashFunction("git.push") {

  object Params {

    val Remote = Parameter(
      name = "remote",
      summary = "Remote to push to",
      defaultValueGeneratorOpt = Some(() ⇒ null))
    val References = Parameter(
      name = "refs",
      summary = "References to push",
      isVariadic = true)

  }
  import Params._

  val params = ParameterModel(Seq(Remote, References))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val refs = boundParams(References).asInstanceOf[MashList]
    val remoteOpt = boundParams.validateStringOpt(Remote)

    GitHelper.withGit { git =>
      val cmd = git.push
      for (ref ← refs)
        cmd.add(ToStringifier.stringify(ref))
      for (remote ← remoteOpt)
        cmd.setRemote(remote.s)
      cmd.call()
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Update remote refs along with associated objects"

}