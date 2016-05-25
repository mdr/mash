package com.github.mdr.mash.parser

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
    case '$'  ⇒ """\$"""
    case '"'  ⇒ """\""""
    case '\\' ⇒ """\\"""
    case '\r' ⇒ """\r"""
    case '\n' ⇒ """\n"""
    case '\t' ⇒ """\t"""
    case c    ⇒ c + ""
  }

  def unescape(s: String): String = {
    val sb = new StringBuilder()
    val it = s.iterator
    while (it.hasNext) {
      it.next() match {
        case '\\' ⇒
          if (it.hasNext)
            sb.append(it.next() match {
              case '\\' ⇒ '\\'
              case 'r'  ⇒ '\r'
              case 'n'  ⇒ '\n'
              case 't'  ⇒ '\t'
              case '"'  ⇒ '"'
              case '\'' ⇒ '\''
              case '$'  ⇒ '$'
              case '~'  ⇒ '~'
              case c    ⇒ "" + '\'' + c
            })
          else
            sb.append('\\')
        case c ⇒
          sb.append(c)
      }
    }
    sb.toString
  }

}