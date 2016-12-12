package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.ObjectModelCreator
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import org.fusesource.jansi.Ansi

class ObjectPrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {
  private val stringifier = new ObjectStringifier(terminalInfo)

  def printObject(obj: MashObject) = {
    val model = new ObjectModelCreator(terminalInfo, viewConfig).create(obj)
    if (model.fields.isEmpty)
      output.println("{}")
    else {
      output.println(stringifier.renderTopRow(model))
      for ((k, v) ← model.fields) {
        val fittedField = Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a(StringUtils.fitToWidth(k, model.fieldColumnWidth)).reset().toString
        val fittedValue = StringUtils.fitToWidth(v, model.valueColumnWidth)
        output.println(stringifier.renderFieldRow(fittedField, fittedValue))
      }
      output.println(stringifier.renderBottomRow(model))
    }
  }

}