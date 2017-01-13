package com.github.mdr.mash.parser

case class DocComment(summary: String, descriptionOpt: Option[String])

object DocCommentParser {

  def parse(s: String): Option[DocComment] =
    PartialFunction.condOpt(splitIntoLines(s) map dropInitialSpace) {
      case first :: rest ⇒
        val summary = first
        val descriptionOpt = if (rest.nonEmpty) Some(rest.mkString("\n")) else None
        DocComment(summary, descriptionOpt)
    }

  private def splitIntoLines(s: String): List[String] = s.split("""\r?\n""", -1).toList

  private def dropInitialSpace(s: String) = if (s startsWith " ") s.tail else s

}