package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line

object FieldHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): Seq[Line] = {
    val help = FieldHelpClass.Wrapper(obj)
    val field = help.field
    Seq(
      renderNameSection(field),
      renderClassSection(help.klass),
      renderDescriptionSection(field.descriptionOpt)).flatten
  }

  private def renderNameSection(field: Field): Seq[Line] =
    renderNameSection("FIELD", Seq(field.name), field.summaryOpt)

}
