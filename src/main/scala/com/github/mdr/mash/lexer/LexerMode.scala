package com.github.mdr.mash.lexer

sealed trait LexerMode

case class NormalMode() extends LexerMode {
  var braceLevel = 0
}

case object MishMode extends LexerMode
case object StringInterpolationMode extends LexerMode
case object StringInterpolationIdentifierMode extends LexerMode
