package com.github.mdr.mash.screen

import com.github.mdr.mash.commands.{ MishCommand, SuffixMishCommand }
import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.lexer.{ MashLexer, Token, TokenType }
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.LineInfo

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LineBufferRenderer {

  private val envInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem

  def renderLineBuffer(state: ReplState, terminalInfo: TerminalInfo): LinesAndCursorPos = {
    val prompt = getPrompt(state.commandNumber, state.mish)
    val lineBuffer = state.lineBuffer
    val unwrappedLines = renderLineBufferChars(lineBuffer.text, prompt, state.mish, state.globalVariables.fields,
      state.bareWords)
    val cursorPos = lineBuffer.cursorPos

    def wrap(line: Line): Seq[Line] = {
      val groups = line.chars.grouped(terminalInfo.columns).toSeq
      for {
        (group, index) ← groups.zipWithIndex
        endsInNewline = index == groups.size - 1
      } yield Line(group, endsInNewline)
    }

    val wrappedLines = unwrappedLines.flatMap(wrap)
    val row = unwrappedLines.take(cursorPos.row).flatMap(wrap).length + (prompt.length + cursorPos.column) / terminalInfo.columns
    val column = (prompt.length + cursorPos.column) % terminalInfo.columns
    LinesAndCursorPos(wrappedLines, Point(row, column))
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
    val concreteProgram = MashParser.parseForgiving(s, mish = mish)
    val provenance = Provenance("not required", s)
    val abstractExpr = new Abstractifier(provenance).abstractify(concreteProgram).body
    BareStringify.getBareTokens(abstractExpr, bindings)
  }

  private def renderLineBufferChars(rawChars: String,
                                    prompt: Seq[StyledCharacter],
                                    mishByDefault: Boolean,
                                    globalVariables: mutable.Map[String, MashValue],
                                    bareWords: Boolean): Seq[Line] = {
    val styledChars = renderChars(rawChars, mishByDefault, globalVariables, bareWords)
    val continuationPrefix = if (prompt.isEmpty) "" else "." * (prompt.length - 1) + " "
    val lineRegions = new LineInfo(rawChars).lineRegions
    lineRegions.zipWithIndex.map {
      case (region, 0) ⇒ Line(prompt ++ region.of(styledChars))
      case (region, _) ⇒ Line(continuationPrefix.style ++ region.of(styledChars))
    }
  }

  def renderChars(rawChars: String, mishByDefault: Boolean, globalVariables: mutable.Map[String, MashValue], bareWords: Boolean): Seq[StyledCharacter] = {
    val styledChars = new ArrayBuffer[StyledCharacter]

    def getTokens(s: String, mish: Boolean) = {
      val bareTokens = getBareTokens(s, mishByDefault, globalVariables)
      val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens
      (tokens, bareTokens)
    }

    val (tokens, bareTokens) =
      rawChars match {
        case SuffixMishCommand(mishCmd, suffix) ⇒
          getTokens(mishCmd, mish = true)
        case MishCommand(prefix, mishCmd)       ⇒
          styledChars ++= prefix.map(StyledCharacter(_, Style(bold = true)))
          getTokens(mishCmd, mish = true)
        case _                                  ⇒
          getTokens(rawChars, mish = mishByDefault)
      }
    for (token ← tokens) {
      val style =
        if (bareTokens contains token)
          if (bareWords) getTokenStyle(TokenType.STRING_LITERAL) else Style(foregroundColour = Colour.Red)
        else
          getTokenStyle(token)

      if (!token.isEof)
        styledChars ++= token.text.style(style)
    }

    rawChars match {
      case SuffixMishCommand(mishCmd, suffix) ⇒
        styledChars ++= suffix.style(Style(bold = true))
      case _                                  ⇒
    }
    styledChars
  }

  private def getTokenStyle(token: Token): Style = getTokenStyle(token.tokenType)

  private def getTokenStyle(tokenType: TokenType): Style = {
    import TokenType._
    tokenType match {
      case COMMENT                ⇒ Style(foregroundColour = Colour.Cyan)
      case NUMBER_LITERAL         ⇒ Style(foregroundColour = Colour.Blue)
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