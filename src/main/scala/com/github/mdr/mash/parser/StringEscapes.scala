package com.github.mdr.mash.parser

import scala.annotation.tailrec

/**
 * @param escapeMap -- map from position in the original string to the position in the escaped string
 */
case class StringEscapeResult(escaped: String, escapeMap: Map[Int, Int])

object StringEscapes {

  def escapeChars(s: String): String = escapeCharsFull(s).escaped

  def escapeCharsFull(s: String): StringEscapeResult = {
    val sb = new StringBuilder
    var escapeMap: Map[Int, Int] = Map()
    for ((c, i) ← s.zipWithIndex) {
      escapeMap += i -> sb.length
      sb.append(escapeChar(c))
    }
    StringEscapeResult(sb.toString, escapeMap)
  }

  private def escapeChar(c: Char): String = c match {
    case '$'  ⇒ "`$"
    case '"'  ⇒ "`\""
    case '`' ⇒ "``"
    case '\r' ⇒ "`r"
    case '\n' ⇒ "`n"
    case '\t' ⇒ "`t"
    case c    ⇒ c + ""
  }

  def unescape(s: String): String = {
    def unescapeChar(c: Char): Char =
      c match {
        case 'r' ⇒ '\r'
        case 'n' ⇒ '\n'
        case 't' ⇒ '\t'
        case c   ⇒ c
      }
    @tailrec
    def recurse(s: List[Char], acc: List[Char] = Nil, processingEscape: Boolean = false): String =
      if (processingEscape)
        s match {
          case Nil       ⇒ ('`' :: acc).reverse.mkString
          case (c :: cs) ⇒ recurse(cs, unescapeChar(c) :: acc)
        }
      else
        s match {
          case Nil          ⇒ acc.reverse.mkString
          case ('`' :: cs) ⇒ recurse(cs, acc, processingEscape = true)
          case (c :: cs)    ⇒ recurse(cs, c :: acc)
        }
    recurse(s.toList)
  }

}