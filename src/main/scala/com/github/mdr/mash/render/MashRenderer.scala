package com.github.mdr.mash.render

import com.github.mdr.mash.commands.{ MishCommand, SuffixMishCommand }
import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.editor.BracketMatcher
import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.parser.{ Abstractifier, ConcreteSyntax, MashParser, Provenance }
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils._

import scala.collection.mutable.ArrayBuffer

object MashRenderer {

  import TokenType._

  def getTokenStyle(tokenType: TokenType): Style =
    tokenType match {
      case COMMENT                                                    ⇒ Style(foregroundColour = DefaultColours.Comment)
      case NUMBER_LITERAL                                             ⇒ Style(foregroundColour = DefaultColours.Purple)
      case IDENTIFIER | MISH_WORD                                     ⇒ Style(foregroundColour = DefaultColours.Foreground)
      case ERROR                                                      ⇒ Style(foregroundColour = DefaultColours.Red)
      case t if t.isFlag                                              ⇒ Style(foregroundColour = DefaultColours.Cyan)
      case t if t.isKeyword                                           ⇒ Style(foregroundColour = DefaultColours.Pink)
      case STRING_LITERAL | STRING_START | STRING_END | STRING_MIDDLE ⇒ Style(foregroundColour = DefaultColours.Yellow)
      case PIPE | PLUS | TIMES | SHORT_EQUALS | PLUS_EQUALS |
           MINUS_EQUALS | TIMES_EQUALS | DIVIDE_EQUALS | LONG_EQUALS |
           NOT_EQUALS | GREATER_THAN | GREATER_THAN_EQUALS |
           LESS_THAN_EQUALS | LESS_THAN | SEMI                        ⇒ Style(foregroundColour = DefaultColours.Orange)
      case AT                                                         ⇒ Style(foregroundColour = DefaultColours.Green)
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
      val lexerResult = MashLexer.tokenise(s, forgiving = true, mish = mish)
      val program = MashParser.parseProgram(lexerResult, forgiving = true, mish = mish)
      val tokens = lexerResult.rawTokens
      val bareTokens = getBareTokens(program)
      val matchingBracketOffsetOpt = cursorOffsetOpt.flatMap(cursorOffset ⇒
        BracketMatcher.findMatchingBracket(program, cursorOffset))
      val declaredNameTokens = getDeclaredNameTokens(program)
      val bareObjectKeyTokens = getBareObjectKeyTokens(program)
      val attributeTokens = getAttributeTokens(program)
      TokenInfo(tokens, bareTokens, matchingBracketOffsetOpt, declaredNameTokens, bareObjectKeyTokens, attributeTokens)
    }

    val TokenInfo(tokens, bareTokens, matchingBracketOffsetOpt, declaredNameTokens, bareObjectKeyTokens, attributeTokens) =
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
      styledChars ++= renderToken(token, bareTokens, declaredNameTokens, bareObjectKeyTokens, attributeTokens, matchingBracketOffsetOpt).chars

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

  private case class TokenInfo(tokens: Seq[Token],
                               bareTokens: Set[Token],
                               matchingBracketOffsetOpt: Option[Int],
                               declaredNameTokens: Set[Token],
                               bareObjectKeyTokens: Set[Token],
                               attributeTokens: Set[Token])

  private def getDeclaredNameTokens(program: ConcreteSyntax.Program): Set[Token] =
    program.findAll {
      case classDecl: ClassDeclaration       ⇒ classDecl.name
      case functionDecl: FunctionDeclaration ⇒ functionDecl.name
    }.toSet

  private def getBareObjectKeyTokens(program: ConcreteSyntax.Program): Set[Token] =
    program.findAll {
      case ShorthandObjectEntry(identifier)              ⇒ identifier
      case FullObjectEntry(Identifier(identifier), _, _) ⇒ identifier
    }.toSet

  private def getAttributeTokens(program: ConcreteSyntax.Program): Set[Token] =
    program.findAll {
      case SimpleAttribute(_, identifier)            ⇒ identifier
      case ArgumentAttribute(_, _, identifier, _, _) ⇒ identifier
    }.toSet

  private def getBareTokens(program: ConcreteSyntax.Program): Set[Token] =
    context.globalVariablesOpt.map { globalVariables ⇒
      val bindings = globalVariables.immutableFields.keySet.collect { case s: MashString ⇒ s.s }
      val provenance = Provenance("not required", "")
      val abstractExpr = new Abstractifier(provenance).abstractify(program).body
      BareStringify.getBareTokens(abstractExpr, bindings)
    }.getOrElse(Set())

  private def renderToken(token: Token,
                          bareTokens: Set[Token],
                          declaredNameTokens: Set[Token],
                          bareObjectKeyTokens: Set[Token],
                          attributeTokens: Set[Token],
                          matchingBracketOffsetOpt: Option[Int]): StyledString = {
    val style =
      if (bareTokens contains token)
        if (context.bareWords) MashRenderer.getTokenStyle(TokenType.STRING_LITERAL) else Style(foregroundColour = BasicColour.Red)
      else if (declaredNameTokens contains token)
        getTokenStyle(token).withBold
      else if (bareObjectKeyTokens contains token)
        MashRenderer.getTokenStyle(TokenType.STRING_LITERAL)
      else if (attributeTokens contains token)
        MashRenderer.getTokenStyle(TokenType.AT)
      else
        getTokenStyle(token)

    val initialTokenChars = token.text.style(style)

    matchingBracketOffsetOpt match {
      case Some(offset) if token.region contains offset ⇒
        val posWithinToken = offset - token.offset
        val newChar = initialTokenChars(posWithinToken).updateStyle(_.copy(underline = true))
        initialTokenChars.updated(posWithinToken, newChar)
      case _                                            ⇒
        initialTokenChars
    }
  }

  def getTokenStyle(token: Token): Style = MashRenderer.getTokenStyle(token.tokenType)

}
