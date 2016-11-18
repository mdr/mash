package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils

class ObjectPrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  def printObject(obj: MashObject) = {
    val model = new ObjectModelCreator(terminalInfo).create(obj)
    if (model.fields.isEmpty)
      output.println("{}")
    else {
      val ObjectModel(fields, keyWidth, valuesWidth, _) = model
      // Top row
      output.print("╔")
      output.print("═" * keyWidth)
      output.print("╤")
      output.print("═" * valuesWidth)
      output.println("╗")

      for ((k, v) ← fields) {
        output.print("║")
        output.print(StringUtils.fitToWidth(k + "", keyWidth))
        output.print("│")
        output.print(StringUtils.fitToWidth(v, valuesWidth))
        output.println("║")
      }

      // Bottom row
      output.print("╚")
      output.print("═" * keyWidth)
      output.print("╧")
      output.print("═" * valuesWidth)
      output.println("╝")
    }
  }

}