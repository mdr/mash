package com.github.mdr.mash.parser

case class StringEscapeResult(escaped: String, escapeMap: Map[Int, Int])

object StringEscapes {

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

  def escapeChars(s: String) = escapeCharsFull(s).escaped

  def unescape(s: String): String = {
    val sb = new StringBuilder()
    val it = s.iterator
    while (it.hasNext) {
      it.next() match {
        case '\\' ⇒
          if (it.hasNext)
            it.next() match {
              case '\\' ⇒
                sb.append('\\')
              case 'r' ⇒
                sb.append('\r')
              case 'n' ⇒
                sb.append('\n')
              case 't' ⇒
                sb.append('\t')
              case '"' ⇒
                sb.append('"')
              case '\'' ⇒
                sb.append('\'')
              case '$' ⇒
                sb.append('$')
              case c ⇒
                sb.append('\'').append(c)
            }
          else
            sb.append('\\')
        case c ⇒
          sb.append(c)
      }
    }
    sb.toString
  }

}