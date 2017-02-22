package com.github.mdr.mash.parser

import com.github.mdr.mash.utils.StringUtils

import scala.PartialFunction.condOpt

case class DocComment(summary: String, descriptionOpt: Option[String], paramComments: Seq[ParamComment]) {

  def getParamComment(paramName: String): Option[ParamComment] = paramComments.find(_.name == paramName)

}

case class ParamComment(name: String, summary: String)

object DocCommentParser {

  private val ParamRegex = """^\s*@param\s+(\w+)\s+(.*)""".r

  private def parseDescription(lines: Seq[String]) =
    if (lines forall (_ matches "\\s*"))
      None
    else
      Some(lines mkString "\n")

  def parse(s: String): Option[DocComment] = {
    val (paramLines, mainLines) = extractParamLines(StringUtils.splitIntoLines(s).toList map dropInitialSpace)
    val (summary, descriptionOpt) = mainLines match {
      case first :: rest ⇒
        val summary = first
        val descriptionOpt = parseDescription(rest)
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

  private def dropInitialSpace(s: String) = if (s startsWith " ") s.tail else s

  private def extractParamLines(lines: List[String]): (List[String], List[String]) =
    lines.partition(_ matches "^\\s*@param.+")
}