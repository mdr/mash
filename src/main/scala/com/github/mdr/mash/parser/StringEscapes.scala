package com.github.mdr.mash.parser

object StringEscapes {

  def escapeChars(s: String) = s.flatMap {
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