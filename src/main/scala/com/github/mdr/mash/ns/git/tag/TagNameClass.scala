package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.core.AnyClass

object TagNameClass extends MashClass("git.tag.TagName") {
  override def summaryOpt = Some("Name of a Git tag")

  override def parentOpt = Some(AnyClass)
}
