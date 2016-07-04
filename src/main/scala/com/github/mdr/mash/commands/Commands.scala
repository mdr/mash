package com.github.mdr.mash.commands

import com.github.mdr.mash.utils.StringUtils

object MishCommand {

  /**
   * Starts with a "!", but isn't a !{} or !!{} expression
   */
  private val MishRegex = """^(\s*!)(?!\{|!\{)(.*)""".r

  def unapply(s: String): Option[(String, String)] =
    MishRegex.unapplySeq(s).map { case Seq(prefix, cmd) ⇒ (prefix, cmd) }

}

object SuffixMishCommand {

  def unapply(s: String): Option[(String, String)] =
    if (s endsWith "!") Some(s.dropRight(1) -> s.takeRight(1)) else None

}

object DebugCommand {

  def unapply(s: String): Option[(String, String)] = {
    val trimmed = StringUtils.ltrim(s)
    if (trimmed startsWith ":") Some(trimmed.tail.span(_.isLetter)) else None
  }

}