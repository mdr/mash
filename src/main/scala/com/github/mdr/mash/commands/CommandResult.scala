package com.github.mdr.mash.commands

import com.github.mdr.mash.printer.ObjectTableModel
import com.github.mdr.mash.runtime.MashValue

case class CommandResult(
  value: Option[MashValue] = None,
  toggleMish: Boolean = false,
  objectTableModelOpt: Option[ObjectTableModel] = None)
