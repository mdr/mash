package com.github.mdr.mash.editor

import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

object SyntaxSelection {

  def expandSelection(lineBuffer: LineBuffer): LineBuffer = {
    val expr = MashParser.parseForgiving(lineBuffer.text).body
    val initialRegion = lineBuffer.selectedOrCursorRegion
    val biggerRegionOpt: Option[Region] =
      expr
        .findAll { case node if node.region contains initialRegion ⇒ node }
        .map(_.region)
        .filter(_.length > initialRegion.length)
        .sortBy(_.length)
        .headOption
    lineBuffer.whenOpt(biggerRegionOpt, expandSelection)
  }

  private def expandSelection(lineBuffer: LineBuffer, newSelectionRegion: Region): LineBuffer = {
    val newCursorOffset = newSelectionRegion.offset
    val newSelectionOffsetOpt = Some(newSelectionRegion.posAfter)
    LineBuffer(lineBuffer.text, newCursorOffset, newSelectionOffsetOpt)
  }

}
