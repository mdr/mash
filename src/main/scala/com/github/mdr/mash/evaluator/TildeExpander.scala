package com.github.mdr.mash.evaluator

import scala.util.matching.Regex
import com.github.mdr.mash.os.EnvironmentInteractions

object TildeExpander {

  private val TildeRegex: Regex = """(?<!\S)~(?=\s|/|$)""".r

}

class TildeExpander(envInteractions: EnvironmentInteractions) {
  import TildeExpander._

  /**
   * Replace ~ with the user's home directory in legal positions
   */
  def expand(s: String): String = TildeRegex.replaceAllIn(s, home)

  /**
   * @return Some(replaced) if a tilde expansion occurred, else None
   */
  def expandOpt(s: String): Option[String] =
    TildeRegex.findFirstMatchIn(s).map { _ ⇒ expand(s) }

  def retilde(s: String): String =
    if (s startsWith home)
      "~" + s.drop(home.length)
    else
      s

  private def home: String = envInteractions.home.toString

}