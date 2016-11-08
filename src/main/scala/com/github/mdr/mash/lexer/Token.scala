package com.github.mdr.mash.lexer

import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils

/**
 * @param source -- entire source string from which the token was taken.
 */
case class Token(tokenType: TokenType, offset: Int, length: Int, source: String) {

  lazy val text = if (tokenType.isEof) "" else source.substring(offset, offset + length)

  def isComment = tokenType.isComment
  def isWhitespace = tokenType.isWhitespace
  def isEof = tokenType.isEof
  def isKeyword = tokenType.isKeyword
  def isLiteral = tokenType.isLiteral
  def isIdentifier = tokenType.isIdentifier
  def isString = tokenType.isString
  def isFlag = tokenType.isFlag

  def region = Region(offset, length)

  override def toString = {
    val abbreviatedText = StringUtils.ellipsisise(text, 20)
    s"""$tokenType($offset, >>$abbreviatedText<<)"""
  }
}
