package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line

object FieldHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): LinesAndLinks = {
    val help = FieldHelpClass.Wrapper(obj)
    val field = help.field
    LinesAndLinks.combine(Seq(
      LinesAndLinks(renderNameSection(field)),
      renderClassSection2(help.klass),
      LinesAndLinks(renderDescriptionSection(field.descriptionOpt))))
  }

  private def renderNameSection(field: Field): Seq[Line] =
    renderNameSection("FIELD", Seq(field.name), field.summaryOpt)

}
