package com.github.mdr.mash.screen

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import com.github.mdr.mash.MishCommand
import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.SuffixMishCommand

case class ReplRenderResult(screen: Screen, completionColumns: Int)

case class LinesAndCursorPos(lines: Seq[Line], cursorPos: Point)

/**
 * Render the current state (input buffer, completion state, assistance information etc) into a set of lines of styled
 * characters.
 */
object ReplRenderer {

  private val envInteractions = LinuxEnvironmentInteractions
  private val fileSystem = LinuxFileSystem

  def render(state: ReplState, terminalInfo: TerminalInfo): ReplRenderResult = state.objectBrowserStateOpt match {
    case Some(objectBrowserState) ⇒
      val screen = ObjectBrowserRenderer.renderObjectBrowser(objectBrowserState, terminalInfo)
      ReplRenderResult(screen, 0)
    case None ⇒
      val bufferScreen = renderLineBuffer(state, terminalInfo)
      val bufferLines = bufferScreen.lines
      val incrementalSearchScreenOpt = state.incrementalSearchStateOpt.map(renderIncrementalSearch(_, terminalInfo))
      val incrementalSearchLines = incrementalSearchScreenOpt.map(_.lines).getOrElse(Seq())
      val assistanceLines = renderAssistanceState(state.assistanceStateOpt, terminalInfo)
      val remainingRows = math.max(0, terminalInfo.rows - bufferLines.size - assistanceLines.size - incrementalSearchLines.size)
      val CompletionRenderResult(completionLines, numberOfCompletionColumns) =
        CompletionRenderer.renderCompletions(state.completionStateOpt, terminalInfo.copy(rows = remainingRows))
      val lines = bufferLines ++ incrementalSearchLines ++ completionLines ++ assistanceLines
      val truncatedLines = lines.take(terminalInfo.rows)
      val newCursorPos = incrementalSearchScreenOpt.map(_.cursorPos.down(bufferLines.size)).getOrElse(bufferScreen.cursorPos)
      val title = fileSystem.pwd.toString
      val screen = Screen(truncatedLines, newCursorPos, cursorVisible = true, title)
      ReplRenderResult(screen, numberOfCompletionColumns)
  }

  private def renderIncrementalSearch(searchState: IncrementalSearchState, terminalInfo: TerminalInfo): LinesAndCursorPos = {
    val prefixChars: Seq[StyledCharacter] = "Incremental history search: ".map(StyledCharacter(_))
    val searchChars: Seq[StyledCharacter] = searchState.searchString.map(c ⇒ StyledCharacter(c, Style(foregroundColour = Colour.Cyan)))
    val chars = (prefixChars ++ searchChars).take(terminalInfo.columns)
    val line = Line((prefixChars ++ searchChars).take(terminalInfo.columns))
    val cursorPos = Point(0, chars.size)
    LinesAndCursorPos(Seq(line), cursorPos)
  }

  private def renderAssistanceState(assistanceStateOpt: Option[AssistanceState], terminalInfo: TerminalInfo): Seq[Line] =
    assistanceStateOpt.map { assistanceState ⇒
      val title = assistanceState.title
      val lines = assistanceState.lines
      val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 4), terminalInfo.columns)
      val innerWidth = boxWidth - 4
      val displayTitle = " " + StringUtils.ellipsisise(title, innerWidth) + " "
      val displayLines = lines.map(l ⇒ StringUtils.ellipsisise(l, innerWidth))
      val topLine = Line(("┌─" + displayTitle + "─" * (innerWidth - displayTitle.length) + "─┐").map(StyledCharacter(_)))
      val bottomLine = Line(("└─" + "─" * innerWidth + "─┘").map(StyledCharacter(_)))
      val contentLines = displayLines.map(l ⇒ Line(("│ " + l + " " * (innerWidth - l.length) + " │").map(StyledCharacter(_))))
      topLine +: contentLines :+ bottomLine
    }.getOrElse(Seq())

  private def getPrompt(commandNumber: Int, mishByDefault: Boolean): Seq[StyledCharacter] = {
    val num = s"[$commandNumber] "
    val numStyle = Style(foregroundColour = Colour.Yellow)
    val numStyled = num.map(StyledCharacter(_, numStyle))

    val pwd = new TildeExpander(envInteractions).retilde(fileSystem.pwd.toString)
    val pwdStyle = Style(foregroundColour = Colour.Cyan, bold = true)
    val pwdStyled = pwd.map(StyledCharacter(_, pwdStyle))

    val promptChar = if (mishByDefault) "!" else "$"
    val promptCharStyle = Style(foregroundColour = Colour.Green, bold = true)
    val promptCharStyled = s" $promptChar ".map(StyledCharacter(_, promptCharStyle))

    numStyled ++ pwdStyled ++ promptCharStyled
  }

  private def renderLineBuffer(state: ReplState, terminalInfo: TerminalInfo): LinesAndCursorPos = {
    val prompt = getPrompt(state.commandNumber, state.mish)
    val lineBuffer = state.lineBuffer
    val styledChars = renderLineBufferChars(lineBuffer.text, prompt, state.mish, state.globalVariables, state.bareWords)
    val cursorPos = prompt.length + lineBuffer.cursorPos

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

  private def getBareTokens(s: String, mish: Boolean, globalVariables: mutable.Map[String, MashValue]): Set[Token] = {
    val bindings = globalVariables.keySet.toSet
    MashParser.parse(s, forgiving = true, mish = mish).map { concreteExpr ⇒
      val abstractExpr = Abstractifier.abstractify(concreteExpr)
      BareStringify.getBareTokens(abstractExpr, bindings)
    }.getOrElse(Set())
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
        styledChars ++= token.text.map(StyledCharacter(_, style))
    }
    rawChars match {
      case SuffixMishCommand(mishCmd, suffix) ⇒
        styledChars ++= suffix.map(StyledCharacter(_, Style(bold = true)))
      case _ ⇒
    }
    styledChars
  }

  private def getTokenStyle(token: Token): Style = getTokenStyle(token.tokenType)

  private def getTokenStyle(tokenType: TokenType): Style = {
    import TokenType._
    tokenType match {
      case COMMENT ⇒ Style(foregroundColour = Colour.Cyan)
      case NUMBER_LITERAL ⇒ Style(foregroundColour = Colour.Yellow)
      case IDENTIFIER | MISH_WORD ⇒ Style(foregroundColour = Colour.Yellow)
      case ERROR ⇒ Style(foregroundColour = Colour.Red, bold = true)
      case t if t.isFlag ⇒ Style(foregroundColour = Colour.Blue, bold = true)
      case t if t.isKeyword ⇒ Style(foregroundColour = Colour.Magenta, bold = true)
      case STRING_LITERAL | STRING_START | STRING_END | STRING_MIDDLE ⇒ Style(foregroundColour = Colour.Green)
      case _ ⇒ Style()
    }
  }

}