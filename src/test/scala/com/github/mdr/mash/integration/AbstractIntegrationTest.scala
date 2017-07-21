package com.github.mdr.mash.integration

import java.io.PrintStream
import java.util.UUID

import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.os.{ FileSystem, MockEnvironmentInteractions, MockFileSystem }
import com.github.mdr.mash.repl.NormalActions.{ AcceptLine, AssistInvocation, BackwardKillLine, SelfInsert }
import com.github.mdr.mash.repl._
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.ExpressionInput.BeginExpression
import com.github.mdr.mash.repl.browser.ObjectBrowserActions.{ PreviousColumn, UnfocusColumn, _ }
import com.github.mdr.mash.repl.browser.{ ObjectBrowserStateStack, SingleObjectTableBrowserState, TwoDTableBrowserState }
import com.github.mdr.mash.repl.completions.IncrementalCompletionState
import com.github.mdr.mash.repl.history.InMemoryHistoryStorage
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.terminal.DummyTerminal
import org.scalatest._

class AbstractIntegrationTest extends FlatSpec with Matchers {

  def makeRepl(fileSystem: FileSystem = new MockFileSystem) = {
    val history = InMemoryHistoryStorage.testHistory()
    val globalVariables = StandardEnvironment.createGlobalVariables()
    object NullPrintStream extends PrintStream(_ => ())
    new Repl(DummyTerminal(), NullPrintStream, fileSystem, new MockEnvironmentInteractions, history = history,
      sessionId = UUID.randomUUID, globalVariables = globalVariables)
  }

  case class SingleObjectBrowser(repl: Repl) {

    def path = getState.path

    def rows: Seq[(String, String)] = getState.model.fields.toSeq

    private def getState: SingleObjectTableBrowserState = repl.getSingleObjectBrowserState

    def nextParentItem(): SingleObjectBrowser = {
      repl.handleAction(NextParentItem)
      this
    }

    def previousParentItem(): SingleObjectBrowser = {
      repl.handleAction(PreviousParentItem)
      this
    }

    def back(): Repl = {
      repl.handleAction(Back)
      repl
    }

  }

  case class TwoDBrowser(repl: Repl) {

    def path = getState.path

    def rows: Seq[Seq[String]] = {
      val model = getState.model
      model.rows.map(row ⇒ model.columnIds.map(columnId ⇒ row.cells(columnId).renderedValue))
    }

    def focus() = {
      repl.handleAction(Focus)
      repl
    }

    def currentRow: Int = getState.currentRow

    def currentColumnOpt: Option[Int] = getState.currentColumnOpt

    def nextColumn() = {
      repl.handleAction(NextColumn)
      this
    }

    def previousColumn() = {
      repl.handleAction(PreviousColumn)
      this
    }

    def unfocusColumn() = {
      repl.handleAction(UnfocusColumn)
      this
    }

    def beginExpression() = {
      repl.handleAction(BeginExpression)
      this
    }

    def backwardKillLine() = {
      repl.handleAction(BackwardKillLine)
      this
    }

    def input(s: String) = {
      repl.handleAction(SelfInsert(s))
      this
    }

    def acceptLine() = {
      repl.handleAction(AcceptLine)
      repl
    }

    def assistInvocation(): Repl = {
      repl.handleAction(AssistInvocation)
      repl
    }

    def back(): Repl = {
      repl.handleAction(Back)
      repl
    }

    private def getState: TwoDTableBrowserState = repl.getTwoDBrowserState

  }

  implicit class RichRepl(repl: Repl) {

    import com.github.mdr.mash.repl.NormalActions._

    def input(s: String): Repl = {
      repl.handleAction(SelfInsert(s))
      repl
    }

    def assistInvocation(): Repl = {
      repl.handleAction(AssistInvocation)
      repl
    }

    def incrementalHistorySearch(): Repl = {
      repl.handleAction(IncrementalHistorySearch)
      repl
    }

    def complete(): Repl = {
      repl.handleAction(Complete)
      repl
    }

    def previousHistory(): Repl = {
      repl.handleAction(PreviousHistory)
      repl
    }

    def nextHistory(): Repl = {
      repl.handleAction(NextHistory)
      repl
    }

    def acceptLine(): Repl = {
      repl.handleAction(AcceptLine)
      repl
    }

    def toggleQuote(): Repl = {
      repl.handleAction(ToggleQuote)
      repl
    }

    def insertLastArgument(): Repl = {
      repl.handleAction(InsertLastArg)
      repl
    }

    def text: String = lineBuffer.text

    def lineBuffer: LineBuffer = repl.state.lineBuffer

    def left(n: Int = 1): Repl = {
      for (i ← 1 to n)
        repl.handleAction(BackwardChar)
      repl
    }

    def delete(): Repl = {
      repl.handleAction(DeleteChar)
      repl
    }

    def backspace(): Repl = {
      repl.handleAction(BackwardDeleteChar)
      repl
    }

    def draw(): Repl = {
      repl.draw()
      repl
    }

    def lastValue: MashValue =
      repl.globalVariables.get(ReplVariables.It).getOrElse(throw new AssertionError("No binding for 'it'"))

    def incrementalCompletionState = repl.state.completionStateOpt.collect {
      case state: IncrementalCompletionState ⇒ state
    }.getOrElse(throw new AssertionError("Not in incremental completion mode"))

    def affirmInTwoDBrowser = {
      getTwoDBrowserState
      TwoDBrowser(repl)
    }

    def affirmInSingleObjectBrowser = {
      getSingleObjectBrowserState
      SingleObjectBrowser(repl)
    }

    def getTwoDBrowserState: TwoDTableBrowserState =
      getBrowserStateStack.headState match {
        case state: TwoDTableBrowserState ⇒ state
        case state                        ⇒ throw new AssertionError(s"Was not a 2D browser, but a ${state.getClass.getSimpleName}")
      }

    private def getBrowserStateStack: ObjectBrowserStateStack =
      repl.state.objectBrowserStateStackOpt.getOrElse(throw new AssertionError("Expected browser, but no browser active"))

    def getSingleObjectBrowserState: SingleObjectTableBrowserState =
      getBrowserStateStack.headState match {
        case state: SingleObjectTableBrowserState ⇒
          state
        case state                                ⇒
          throw new AssertionError(s"Was not a single object browser, but a ${state.getClass.getSimpleName}")
      }
  }

}







