package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

import scala.collection.mutable.ArrayBuffer

object FieldHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()
    val fieldHelp = FieldHelpClass.Wrapper(obj)
    lines += Line(SectionTitleStyle("FIELD"))
    lines += Line(IndentSpace + NameStyle(fieldHelp.name) + fieldHelp.summaryOpt.fold("")(" - " + _).style)
    lines += Line.Empty
    lines += Line(SectionTitleStyle("CLASS"))
    lines += Line(IndentSpace + fieldHelp.klass.style)
    for (description ← fieldHelp.descriptionOpt) {
      lines += Line.Empty
      lines += Line(SectionTitleStyle("DESCRIPTION"))
      lines ++= DescriptionRenderer.render(description)
    }
    lines
  }

}
