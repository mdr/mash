package com.github.mdr.mash.lexer

import scala.annotation._
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.lexer.TokenType._

sealed trait LexerMode

case class NormalMode() extends LexerMode {
  var braceLevel = 0
}

case object MishMode extends LexerMode
case object StringInterpolationMode extends LexerMode
case object StringInterpolationIdentifierMode extends LexerMode
