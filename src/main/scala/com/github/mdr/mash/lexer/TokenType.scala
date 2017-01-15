package com.github.mdr.mash.lexer

abstract sealed class TokenType {

  import TokenType._

  def isWhitespace = this == WHITESPACE
  def isComment = this == COMMENT
  def isIdentifier = this == IDENTIFIER
  def isString = this == STRING_LITERAL
  def isEof = this == EOF
  def isKeyword = Keywords contains this
  def isLiteral = Literals contains this
  def isFlag = this == SHORT_FLAG || this == LONG_FLAG

}

object TokenType {

  val Keywords: Set[TokenType] = Set(TRUE, FALSE, AND, OR, IF, THEN, ELSE, NULL, DEF, CLASS, ALIAS, LAZY, THIS, NAMESPACE)
  val Literals: Set[TokenType] = Set(TRUE, FALSE, NUMBER_LITERAL, STRING_LITERAL, NULL)

  case object WHITESPACE extends TokenType
  case object IDENTIFIER extends TokenType
  case object COMMENT extends TokenType
  case object LONG_FLAG extends TokenType // --flagName
  case object SHORT_FLAG extends TokenType // -flagName
  case object SHORT_EQUALS extends TokenType // =
  case object LONG_EQUALS extends TokenType // ==
  case object NOT_EQUALS extends TokenType // !=
  case object GREATER_THAN_EQUALS extends TokenType
  case object GREATER_THAN extends TokenType
  case object LESS_THAN_EQUALS extends TokenType
  case object LESS_THAN extends TokenType
  case object RIGHT_ARROW extends TokenType // ⇒
  case object PLUS extends TokenType // +
  case object MINUS extends TokenType // -
  case object TIMES extends TokenType // *
  case object DIVIDE extends TokenType // /
  case object PLUS_EQUALS extends TokenType // +=
  case object MINUS_EQUALS extends TokenType // -=
  case object TIMES_EQUALS extends TokenType // *=
  case object DIVIDE_EQUALS extends TokenType // /=
  case object PIPE extends TokenType // |
  case object LPAREN extends TokenType // (
  case object LPAREN_INVOKE extends TokenType // foo(
  case object RPAREN extends TokenType // )
  case object LBRACE extends TokenType // {
  case object RBRACE extends TokenType // }
  case object LSQUARE extends TokenType // [
  case object RSQUARE extends TokenType // ]
  case object LSQUARE_LOOKUP extends TokenType // foo[  (blah)[ "foo"[3]  
  case object COMMA extends TokenType // ]
  case object COLON extends TokenType // :
  case object DOT extends TokenType
  case object DOT_NULL_SAFE extends TokenType // ?.
  case object ELLIPSIS extends TokenType // ...
  case object HOLE extends TokenType // _
  case object TRUE extends TokenType
  case object FALSE extends TokenType
  case object STRING_LITERAL extends TokenType
  case object NUMBER_LITERAL extends TokenType
  case object AND extends TokenType
  case object OR extends TokenType
  case object IF extends TokenType
  case object THEN extends TokenType
  case object ELSE extends TokenType
  case object SEMI extends TokenType
  case object NULL extends TokenType
  case object TILDE extends TokenType // ~
  case object STRING_START extends TokenType // "foo
  case object STRING_MIDDLE extends TokenType // bar
  case object STRING_END extends TokenType // baz"
  case object STRING_INTERPOLATION_START_COMPLEX extends TokenType // ${
  case object STRING_INTERPOLATION_START_SIMPLE extends TokenType // $
  case object EOF extends TokenType
  case object MISH_WORD extends TokenType
  case object MISH_INTERPOLATION_START extends TokenType // !{
  case object MISH_INTERPOLATION_START_NO_CAPTURE extends TokenType // !!{
  case object DEF extends TokenType
  case object QUESTION extends TokenType
  case object ERROR extends TokenType
  case object ALIAS extends TokenType
  case object LAZY extends TokenType
  case object CLASS extends TokenType
  case object THIS extends TokenType
  case object NAMESPACE extends TokenType
}
