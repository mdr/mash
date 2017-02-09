
package com.github.mdr.mash.ns.git.remote

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.core.AnyClass

object RemoteNameClass extends MashClass("git.branch.RemoteName") {

  override def summaryOpt = Some("A git remote name")

  override def parentOpt = Some(AnyClass)

}

