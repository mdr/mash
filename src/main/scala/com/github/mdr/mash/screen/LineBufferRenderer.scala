package com.github.mdr.mash.screen

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import com.github.mdr.mash.commands.MishCommand
import com.github.mdr.mash.commands.SuffixMishCommand
import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Provenance
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo

object LineBufferRenderer {

  private val envInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem

  def renderLineBuffer(state: ReplState, terminalInfo: TerminalInfo): LinesAndCursorPos = {
    val prompt = getPrompt(state.commandNumber, state.mish)
    val lineBuffer = state.lineBuffer
    val styledChars = renderLineBufferChars(lineBuffer.text, prompt, state.mish, state.globalVariables.fields, state.bareWords)
    val cursorPos = prompt.length + lineBuffer.cursorOffset

    val groups = styledChars.grouped(terminalInfo.columns).toSeq
    val lines: Seq[Line] =
      for {
        (group, index) ← groups.zipWithIndex
        endsInNewline = index == groups.size - 1
      } yield Line(group, endsInNewline)
    val row = cursorPos / terminalInfo.columns
    val column = cursorPos % terminalInfo.columns
    LinesAndCursorPos(lines, Point(row, column))
  }

  private def getPrompt(commandNumber: Int, mishByDefault: Boolean): Seq[StyledCharacter] = {
    val num = s"[$commandNumber] "
    val numStyle = Style(foregroundColour = Colour.Yellow)
    val numStyled = num.style(numStyle)

    val pwd = new TildeExpander(envInteractions).retilde(fileSystem.pwd.toString)
    val pwdStyle = Style(foregroundColour = Colour.Cyan, bold = true)
    val pwdStyled = pwd.style(pwdStyle)

    val promptChar = if (mishByDefault) "!" else "$"
    val promptCharStyle = Style(foregroundColour = Colour.Green, bold = true)
    val promptCharStyled = s" $promptChar ".style(promptCharStyle)

    numStyled ++ pwdStyled ++ promptCharStyled
  }

  private def getBareTokens(s: String, mish: Boolean, globalVariables: mutable.Map[String, MashValue]): Set[Token] = {
    val bindings = globalVariables.keySet.toSet
    val concreteExpr = MashParser.parseForgiving(s, mish = mish)
    val provenance = Provenance("not required", s)
    val abstractExpr = new Abstractifier(provenance).abstractify(concreteExpr)
    BareStringify.getBareTokens(abstractExpr, bindings)
  }

  private def renderLineBufferChars(rawChars: String, prompt: Seq[StyledCharacter], mishByDefault: Boolean, globalVariables: mutable.Map[String, MashValue], bareWords: Boolean): Seq[StyledCharacter] = {
    val styledChars = new ArrayBuffer[StyledCharacter]
    styledChars ++= prompt

    val (tokens, bareTokens) =
      rawChars match {
        case SuffixMishCommand(mishCmd, suffix) ⇒
          val bareTokens = if (bareWords) getBareTokens(mishCmd, mishByDefault, globalVariables) else Set[Token]()
          (MashLexer.tokenise(mishCmd, includeCommentsAndWhitespace = true, forgiving = true, mish = true), bareTokens)
        case MishCommand(prefix, mishCmd) ⇒
          styledChars ++= prefix.map(StyledCharacter(_, Style(bold = true)))
          val bareTokens = if (bareWords) getBareTokens(mishCmd, mishByDefault, globalVariables) else Set[Token]()
          (MashLexer.tokenise(mishCmd, includeCommentsAndWhitespace = true, forgiving = true, mish = true), bareTokens)
        case _ ⇒
          val bareTokens = if (bareWords) getBareTokens(rawChars, mishByDefault, globalVariables) else Set[Token]()
          (MashLexer.tokenise(rawChars, includeCommentsAndWhitespace = true, forgiving = true, mish = mishByDefault), bareTokens)
      }
    for (token ← tokens) {
      val style =
        if (bareTokens contains token)
          getTokenStyle(TokenType.STRING_LITERAL)
        else
          getTokenStyle(token)

      if (!token.isEof)
        styledChars ++= token.text.style(style)
    }
    rawChars match {
      case SuffixMishCommand(mishCmd, suffix) ⇒
        styledChars ++= suffix.style(Style(bold = true))
      case _ ⇒
    }
    styledChars
  }

  private def getTokenStyle(token: Token): Style = getTokenStyle(token.tokenType)

  private def getTokenStyle(tokenType: TokenType): Style = {
    import TokenType._
    tokenType match {
      case COMMENT                ⇒ Style(foregroundColour = Colour.Cyan)
      case NUMBER_LITERAL         ⇒ Style(foregroundColour = Colour.Yellow)
      case IDENTIFIER | MISH_WORD ⇒ Style(foregroundColour = Colour.Yellow)
      case ERROR                  ⇒ Style(foregroundColour = Colour.Red, bold = true)
      case t if t.isFlag          ⇒ Style(foregroundColour = Colour.Blue, bold = true)
      case t if t.isKeyword       ⇒ Style(foregroundColour = Colour.Magenta, bold = true)
      case STRING_LITERAL | STRING_START | STRING_END | STRING_MIDDLE ⇒
        Style(foregroundColour = Colour.Green)
      case _ ⇒ Style()
    }
  }

}