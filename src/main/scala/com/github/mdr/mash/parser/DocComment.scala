package com.github.mdr.mash.parser

import scala.PartialFunction.condOpt

case class DocComment(summary: String, descriptionOpt: Option[String], paramComments: Seq[ParamComment]) {

  def getParamComment(paramName: String): Option[ParamComment] = paramComments.find(_.name == paramName)

}

case class ParamComment(name: String, summary: String)

object DocCommentParser {

  private val ParamRegex = """^\s*@param\s+(\w+)\s+(.*)""".r

  def parse(s: String): Option[DocComment] = {
    val (paramLines, mainLines) = extractParamLines(splitIntoLines(s) map dropInitialSpace)
    val (summary, descriptionOpt) = mainLines match {
      case first :: rest ⇒
        val summary = first
        val descriptionOpt = if (rest.nonEmpty) Some(rest.mkString("\n")) else None
        (summary, descriptionOpt)
      case Nil           ⇒
        ("", None)
    }
    val paramComments = paramLines.flatMap(parseParamComment)
    Some(DocComment(summary, descriptionOpt, paramComments))
  }

  private def parseParamComment(line: String): Option[ParamComment] = condOpt(line) {
    case ParamRegex(name, summary) ⇒ ParamComment(name, summary)
  }

  private def splitIntoLines(s: String): List[String] = s.split("""\r?\n""", -1).toList

  private def dropInitialSpace(s: String) = if (s startsWith " ") s.tail else s

  private def extractParamLines(lines: List[String]): (List[String], List[String]) =
    lines.partition(_ matches "^\\s*@param.+")
}