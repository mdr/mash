package com.github.mdr.mash.screen

import com.github.mdr.mash.commands.{ MishCommand, SuffixMishCommand }
import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.editor.BracketMatcher
import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Style.StylableString

import scala.collection.mutable.ArrayBuffer

class MashRenderer(globalVariablesOpt: Option[MashObject] = None, bareWords: Boolean = false) {

  def renderChars(rawChars: String,
                  cursorOffset: Int,
                  mishByDefault: Boolean): StyledString = {
    val styledChars = new ArrayBuffer[StyledCharacter]

    def getTokenInformation(s: String, mish: Boolean): TokenInfo = {
      val bareTokensOpt = getBareTokens(s, mishByDefault)
      val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens
      val matchingBracketOffsetOpt = BracketMatcher.findMatchingBracket(rawChars, cursorOffset, mish = mish)
      TokenInfo(tokens, bareTokensOpt, matchingBracketOffsetOpt)
    }

    val TokenInfo(tokens, bareTokensOpt, matchingBracketOffsetOpt) =
      rawChars match {
        case SuffixMishCommand(mishCmd, suffix) ⇒
          getTokenInformation(mishCmd, mish = true)
        case MishCommand(prefix, mishCmd)       ⇒
          styledChars ++= prefix.map(StyledCharacter(_, Style(bold = true)))
          getTokenInformation(mishCmd, mish = true)
        case _                                  ⇒
          getTokenInformation(rawChars, mish = mishByDefault)
      }

    for (token ← tokens)
      styledChars ++= renderToken(token, bareTokensOpt, matchingBracketOffsetOpt, bareWords).chars

    rawChars match {
      case SuffixMishCommand(mishCmd, suffix) ⇒
        styledChars ++= suffix.style(Style(bold = true)).chars
      case _                                  ⇒
    }
    StyledString(styledChars)
  }

  private def getBareTokens(s: String, mish: Boolean): Option[Set[Token]] =
    globalVariablesOpt.map { globalVariables ⇒
      val bindings = globalVariables.immutableFields.keySet
      val concreteProgram = MashParser.parseForgiving(s, mish = mish)
      val provenance = Provenance("not required", s)
      val abstractExpr = new Abstractifier(provenance).abstractify(concreteProgram).body
      BareStringify.getBareTokens(abstractExpr, bindings)
    }

  private case class TokenInfo(tokens: Seq[Token], bareTokensOpt: Option[Set[Token]], matchingBracketOffsetOpt: Option[Int])

  private def renderToken(token: Token,
                          bareTokensOpt: Option[Set[Token]],
                          matchingBracketOffsetOpt: Option[Int],
                          bareWords: Boolean): StyledString = {
    val style =
      if (bareTokensOpt exists (_ contains token))
        if (bareWords) getTokenStyle(TokenType.STRING_LITERAL) else Style(foregroundColour = Colour.Red)
      else
        getTokenStyle(token)

    val initialTokenChars = token.text.style(style)

    matchingBracketOffsetOpt match {
      case Some(offset) if token.region contains offset ⇒
        val posWithinToken = offset - token.offset
        val newChar = initialTokenChars(posWithinToken).updateStyle(_.copy(foregroundColour = Colour.Cyan, inverse = true))
        initialTokenChars.updated(posWithinToken, newChar)
      case _                                            ⇒
        initialTokenChars
    }
  }

  private def getTokenStyle(token: Token): Style = getTokenStyle(token.tokenType)

  private def getTokenStyle(tokenType: TokenType): Style = {
    import TokenType._
    tokenType match {
      case COMMENT                                                    ⇒ Style(foregroundColour = Colour.Cyan)
      case NUMBER_LITERAL                                             ⇒ Style(foregroundColour = Colour.Blue)
      case IDENTIFIER | MISH_WORD                                     ⇒ Style(foregroundColour = Colour.Yellow)
      case ERROR                                                      ⇒ Style(foregroundColour = Colour.Red, bold = true)
      case t if t.isFlag                                              ⇒ Style(foregroundColour = Colour.Blue, bold = true)
      case t if t.isKeyword                                           ⇒ Style(foregroundColour = Colour.Magenta, bold = true)
      case STRING_LITERAL | STRING_START | STRING_END | STRING_MIDDLE ⇒
        Style(foregroundColour = Colour.Green)
      case _                                                          ⇒ Style()
    }
  }


}
