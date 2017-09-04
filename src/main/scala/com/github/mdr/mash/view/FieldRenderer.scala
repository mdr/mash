package com.github.mdr.mash.view

import java.time.{ Instant, ZoneId, ZonedDateTime }
import java.time.format.{ DateTimeFormatter, FormatStyle }
import java.util.Date

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.os.{ PermissionsClass, PermissionsSectionClass }
import com.github.mdr.mash.ns.time.{ MillisecondsClass, SecondsClass }
import com.github.mdr.mash.printer.{ BytesPrinter, PermissionsPrinter }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.NumberUtils

class FieldRenderer(viewConfig: ViewConfig) {

  private val dateTimeFormatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM)

  def renderField(value: MashValue, inCell: Boolean = false): String = value match {
    case MashBoolean.True if inCell                       ⇒ "✓"
    case MashBoolean.False if inCell                      ⇒ "✗"
    case obj@MashObject(_, Some(PermissionsClass))        ⇒ PermissionsPrinter.permissionsString(obj)
    case obj@MashObject(_, Some(PermissionsSectionClass)) ⇒ PermissionsPrinter.permissionsSectionString(obj)
    case MashNumber(n, Some(BytesClass))                  ⇒ BytesPrinter.humanReadable(n)
    case MashNumber(n, Some(MillisecondsClass))           ⇒ NumberUtils.prettyString(n) + "ms"
    case MashNumber(n, Some(SecondsClass))                ⇒ NumberUtils.prettyString(n) + "s"
    case MashNumber(n, _)                                 ⇒ NumberUtils.prettyString(n)
    case MashWrapped(i: Instant) if viewConfig.fuzzyTime  ⇒ Viewer.prettyTime.format(Date.from(i))
    case MashWrapped(i: Instant)                          ⇒ dateTimeFormatter.format(ZonedDateTime.ofInstant(i, ZoneId.systemDefault))
    case xs: MashList if inCell                           ⇒ xs.elements.map(renderField(_, inCell = inCell)).mkString(", ")
    case xs: MashList                                     ⇒ xs.elements.map(renderField(_, inCell = inCell)).mkString("[", ", ", "]")
    case _                                                ⇒
      val s = ToStringifier.safeStringify(value)
      if (inCell) Viewer.replaceProblematicChars(s) else s
  }

}
