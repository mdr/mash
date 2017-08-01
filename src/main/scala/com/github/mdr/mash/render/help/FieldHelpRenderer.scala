package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.printer.model.{ Link, LinkPath }
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Region

import scala.PartialFunction.condOpt

object FieldHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): LinesAndLinks = {
    val help = FieldHelpClass.Wrapper(obj)
    val field = help.field
    LinesAndLinks.combine(Seq(
      renderNameSection(field),
      renderClassSection(help.klass),
      renderTypeSection(field),
      renderDescriptionSection(field.descriptionOpt)))
  }

  private def renderNameSection(field: Field): LinesAndLinks =
    renderNameSection("FIELD", Seq(field.name), field.summaryOpt)

  private def renderTypeSection(field: Field): LinesAndLinks =
    describeType(field.fieldType) match {
      case Some(klass) ⇒
        val fullyQualifiedName = klass.fullyQualifiedName.toString
        val renderedType = fullyQualifiedName
        val lines = Seq(
          Line.Empty,
          Line(SectionTitleStyle("TYPE")),
          Line(IndentSpace + renderedType.style))
        val linkPath = LinkPath.Absolute(fullyQualifiedName)
        val typeLink = Link(line = 2, Region(IndentSpace.length, renderedType.length), klass, linkPath)
        LinesAndLinks(lines, Seq(typeLink))
      case None        ⇒
        LinesAndLinks.Empty
    }

  private def describeType(fieldType: Type): Option[MashClass] = condOpt(fieldType) {
    case Type.Instance(klass) if klass != AnyClass ⇒ klass
    case Type.Tagged(_, tagClass)                  ⇒ tagClass
  }
}
