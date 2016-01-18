package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass

object CommitHashClass extends MashClass("git.CommitHash") {

  override val fields = Seq()

  def summary = "A git commit SHA-1 hash"

}