package com.github.mdr.mash.render

import com.github.mdr.mash.commands.{ MishCommand, SuffixMishCommand }
import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.editor.BracketMatcher
import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ BasicColour, Style, StyledCharacter, StyledString }
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

object MashRenderer {

  import TokenType._

  def getTokenStyle(tokenType: TokenType): Style =
    tokenType match {
      case COMMENT                                                    ⇒ Style(foregroundColour = BasicColour.Cyan)
      case NUMBER_LITERAL                                             ⇒ Style(foregroundColour = BasicColour.Blue)
      case IDENTIFIER | MISH_WORD                                     ⇒ Style(foregroundColour = BasicColour.Yellow.bright)
      case ERROR                                                      ⇒ Style(foregroundColour = BasicColour.Red, bold = true)
      case t if t.isFlag                                              ⇒ Style(foregroundColour = BasicColour.Blue.bright)
      case t if t.isKeyword                                           ⇒ Style(foregroundColour = BasicColour.Magenta, bold = true)
      case STRING_LITERAL | STRING_START | STRING_END | STRING_MIDDLE ⇒ Style(foregroundColour = BasicColour.Green.bright)
      case _                                                          ⇒ Style()
    }

}

case class MashRenderingContext(globalVariablesOpt: Option[MashObject] = None,
                                bareWords: Boolean = false,
                                mishByDefault: Boolean = false)

class MashRenderer(context: MashRenderingContext = MashRenderingContext()) {

  def renderChars(rawChars: String,
                  cursorOffsetOpt: Option[Int] = None,
                  matchRegionOpt: Option[Region] = None): StyledString = {
    val styledChars = new ArrayBuffer[StyledCharacter]

    def getTokenInformation(s: String, mish: Boolean): TokenInfo = {
      val bareTokensOpt = getBareTokens(s, mish)
      val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens
      val matchingBracketOffsetOpt = cursorOffsetOpt.flatMap(cursorOffset ⇒
        BracketMatcher.findMatchingBracket(rawChars, cursorOffset, mish = mish))
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
          getTokenInformation(rawChars, mish = context.mishByDefault)
      }

    for (token ← tokens)
      styledChars ++= renderToken(token, bareTokensOpt, matchingBracketOffsetOpt, context.bareWords).chars

    rawChars match {
      case SuffixMishCommand(mishCmd, suffix) ⇒
        styledChars ++= suffix.style(Style(bold = true)).chars
      case _                                  ⇒
    }
    val restyledChars =
      for ((c, i) ← styledChars.zipWithIndex)
        yield c.when(matchRegionOpt.exists(_ contains i), _.updateStyle(_.withUnderline))
    StyledString(restyledChars)
  }

  private def getBareTokens(s: String, mish: Boolean): Option[Set[Token]] =
    context.globalVariablesOpt.map { globalVariables ⇒
      val bindings = globalVariables.immutableFields.keySet.collect { case s: MashString ⇒ s.s }
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
        if (bareWords) MashRenderer.getTokenStyle(TokenType.STRING_LITERAL) else Style(foregroundColour = BasicColour.Red)
      else
        getTokenStyle(token)

    val initialTokenChars = token.text.style(style)

    matchingBracketOffsetOpt match {
      case Some(offset) if token.region contains offset ⇒
        val posWithinToken = offset - token.offset
        val newChar = initialTokenChars(posWithinToken).updateStyle(_.copy(foregroundColour = BasicColour.Cyan, inverse = true))
        initialTokenChars.updated(posWithinToken, newChar)
      case _                                            ⇒
        initialTokenChars
    }
  }

  def getTokenStyle(token: Token): Style = MashRenderer.getTokenStyle(token.tokenType)

}
