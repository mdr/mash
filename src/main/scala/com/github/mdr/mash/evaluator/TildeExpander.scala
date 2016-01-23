package com.github.mdr.mash.evaluator

import scala.util.matching.Regex
import com.github.mdr.mash.os.EnvironmentInteractions

object TildeExpander {

  private val TildeRegex: Regex = "(?<!\\S)~(?=\\s|/|$)".r

}

class TildeExpander(envInteractions: EnvironmentInteractions) {
  import TildeExpander._

  def expand(s: String): String = TildeRegex.replaceAllIn(s, home)

  def expandOpt(s: String): Option[String] =
    TildeRegex.findFirstMatchIn(s).map { _ ⇒ expand(s) }

  private def home = envInteractions.home.toString

  def retilde(s: String): String = {
    if (s startsWith home)
      "~" + s.drop(home.length)
    else
      s
  }

}