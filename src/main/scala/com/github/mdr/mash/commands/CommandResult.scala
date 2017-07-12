package com.github.mdr.mash.commands

import com.github.mdr.mash.printer.model.DisplayModel
import com.github.mdr.mash.runtime.MashValue

case class CommandResult(value: Option[MashValue] = None,
                         toggleMish: Boolean = false,
                         displayModelOpt: Option[DisplayModel] = None)
