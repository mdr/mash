package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.evaluator.MashClass

object TagNameClass extends MashClass("git.tag.TagName") {
  override def summary: String = "Name of a Git tag"
}
