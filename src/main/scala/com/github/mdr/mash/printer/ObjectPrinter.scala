package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.SingleObjectTableModelCreator
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import org.fusesource.jansi.Ansi

class ObjectPrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {
  private val stringifier = new SingleObjectTableStringifier(terminalInfo)

  def printObject(obj: MashObject) = {
    val model = new SingleObjectTableModelCreator(terminalInfo, viewConfig).create(obj)
    if (model.fields.isEmpty)
      output.println("{}")
    else {
      output.println(stringifier.renderTopRow(model))
      for ((k, v) ← model.fields) {
        val fittedField = StringUtils.fitToWidth(k, model.fieldColumnWidth)
        val renderedField = Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a(fittedField).reset().toString
        val fittedValue = StringUtils.fitToWidth(v, model.valueColumnWidth)
        output.println(stringifier.renderFieldRow(renderedField, fittedValue))
      }
      output.println(stringifier.renderBottomRow(model))
    }
  }

}