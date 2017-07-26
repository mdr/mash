package com.github.mdr.mash.render.help

import java.util.regex.Pattern

import com.github.mdr.mash.render.MashRenderer
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ Line, StyledString }
import com.github.mdr.mash.utils.{ LineInfo, Region }

import scala.collection.mutable.ArrayBuffer

/**
  * Render descriptions, including marking up <mash>regions</mash> with syntax highlighting.
  */
object DescriptionRenderer {

  private val MashMarkupPattern = Pattern.compile("<mash>(.+?)</mash>", Pattern.DOTALL)

  def renderDescription(s: String, indentLevel: Int = 1): Seq[Line] = {

    val renderedDescription: StyledString = renderIntoASingleString(s, indentLevel)
    new LineInfo(renderedDescription.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(HelpRenderer.IndentSpace * indentLevel + region.of(renderedDescription.chars)))
  }

  private def renderIntoASingleString(s: String, indentLevel: Int): StyledString = {
    val matcher = MashMarkupPattern.matcher(s)
    val chunks = ArrayBuffer[StyledString]()
    var previous = 0
    while (matcher.find()) {
      val region = Region.fromStartEnd(previous, matcher.start)
      chunks += region.of(s).style
      previous = matcher.end
      val contents = matcher.group(1)
      val renderedMash = new MashRenderer().renderChars(trimInitialAndFinalNewlines(contents))
      chunks += renderedMash
    }
    chunks += s.substring(previous).style
    StyledString.Empty.join(chunks)
  }

  private def trimInitialAndFinalNewlines(contents: String): String = {
    var inner = contents
    if (inner startsWith "\r")
      inner = inner.tail
    if (inner startsWith "\n")
      inner = inner.tail
    if (inner endsWith "\n")
      inner = inner.init
    if (inner endsWith "\r")
      inner = inner.init
    inner
  }
}
