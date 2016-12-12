package com.github.mdr.mash.commands

import com.github.mdr.mash.printer.model.PrintModel
import com.github.mdr.mash.runtime.MashValue

case class CommandResult(value: Option[MashValue] = None,
                         toggleMish: Boolean = false,
                         printModelOpt: Option[PrintModel] = None)
