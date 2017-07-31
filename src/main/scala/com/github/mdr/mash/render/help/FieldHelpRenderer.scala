package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.runtime.MashObject

object FieldHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): LinesAndLinks = {
    val help = FieldHelpClass.Wrapper(obj)
    val field = help.field
    LinesAndLinks.combine(Seq(
      renderNameSection(field),
      renderClassSection(help.klass),
      renderDescriptionSection(field.descriptionOpt)))
  }

  private def renderNameSection(field: Field): LinesAndLinks =
    renderNameSection("FIELD", Seq(field.name), field.summaryOpt)

}
