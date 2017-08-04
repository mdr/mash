package com.github.mdr.mash.editor

import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

object SyntaxSelection {

  def expandSelection(lineBuffer: LineBuffer): LineBuffer = {
    val expr = MashParser.parseForgiving(lineBuffer.text).body
    val initialRegion = lineBuffer.selectedOrCursorRegion
    val nextBiggestRegionOpt: Option[Region] =
      expr
        .findAllMatching(_.region contains initialRegion)
        .map(_.region)
        .filter(_.length > initialRegion.length)
        .sortBy(_.length)
        .headOption
    lineBuffer.whenOpt(nextBiggestRegionOpt, expandSelection)
  }

  private def expandSelection(lineBuffer: LineBuffer, newSelectionRegion: Region): LineBuffer = {
    val newCursorOffset = newSelectionRegion.posAfter
    val newSelectionOffsetOpt = Some(newSelectionRegion.offset)
    LineBuffer(lineBuffer.text, newCursorOffset, newSelectionOffsetOpt)
  }

}
