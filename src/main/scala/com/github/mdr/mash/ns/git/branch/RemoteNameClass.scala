
package com.github.mdr.mash.ns.git.branch

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.core.AnyClass

object RemoteNameClass extends MashClass("git.branch.RemoteName") {

  def summary = "A git remote name"

  override def parentOpt = Some(AnyClass)

}

