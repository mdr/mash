package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass

object NoArgClass extends MashClass("core.NoArg") {

  override def summaryOpt = Some("Value that represents no argument being provided, for use as sentinel values in default arguments")

}
