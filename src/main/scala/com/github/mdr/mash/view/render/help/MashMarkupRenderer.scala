package com.github.mdr.mash.view.render.help

import java.util.regex.Pattern

import com.github.mdr.mash.view.render.MashRenderer
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.StyledString
import com.github.mdr.mash.utils.Region

import scala.collection.mutable.ArrayBuffer

/**
  * Render a string that might include <mash>regions</mash> with syntax highlighting.
  */
object MashMarkupRenderer {

  private val MashMarkupPattern = Pattern.compile("<mash>(.+?)</mash>", Pattern.DOTALL)

  def render(s: String): StyledString = {
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
    StyledString.join(chunks)
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
