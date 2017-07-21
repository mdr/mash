package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.repl.IncrementalHistorySearchState.{ AfterLastHit, BeforeFirstHit, Hit }
import com.github.mdr.mash.repl.LineBufferTestHelper._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.handler.IncrementalHistorySearchActionHandler.Result
import com.github.mdr.mash.repl.history.InMemoryHistoryStorage
import com.github.mdr.mash.repl.{ IncrementalHistorySearchState, ReplState }
import com.github.mdr.mash.utils.Region
import org.scalatest.{ FlatSpec, Matchers }

class IncrementalHistorySearchActionHandlerTest extends FlatSpec with Matchers {

  "Incrementally searching history" should "allow cycling through all matches in history, starting with most recent" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 should equal(replState("existing▶").withHistorySearchState(""))

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("b"), state1)
    state2 should equal(replState("baz▶").withHistorySearchState("b", 0, Region(0, 1)))

    val Result(state3, true) = actionHandler.handleAction(IncrementalHistorySearch, state2)
    state3 should equal(replState("bar▶").withHistorySearchState("b", 1, Region(0, 1)))

    val Result(state4, true) = actionHandler.handleAction(IncrementalHistorySearch, state3)
    state4 should equal(replState("▶").withHistorySearchStateAfterLastHit("b"))

    val Result(state5, true) = actionHandler.handleAction(IncrementalHistorySearch, state4)
    state5 should equal(state4)
  }

  it should "allow characters to be deleted from the search" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 should equal(replState("existing▶").withHistorySearchState(""))

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("a"), state1)
    state2 should equal(replState("baz▶").withHistorySearchState("a", 0, Region(1, 1)))

    val Result(state3, true) = actionHandler.handleAction(SelfInsert("r"), state2)
    state3 should equal(replState("bar▶").withHistorySearchState("ar", 0, Region(1, 2)))

    val Result(state4, true) = actionHandler.handleAction(BackwardDeleteChar, state3)
    state4 should equal(replState("baz▶").withHistorySearchState("a", 0, Region(1, 1)))

    val Result(state5, true) = actionHandler.handleAction(BackwardDeleteChar, state4)
    state5 should equal(replState("▶").withHistorySearchState(""))

    val Result(state6, true) = actionHandler.handleAction(BackwardDeleteChar, state5)
    state6 should equal(replState("▶"))
  }

  it should "allow the user to accept a search result and exit search" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 should equal(replState("existing▶").withHistorySearchState(""))

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("f"), state1)
    state2 should equal(replState("foo▶").withHistorySearchState("f", 0, Region(0, 1)))

    val Result(state3, true) = actionHandler.handleAction(AcceptLine, state2)
    state3 should equal(replState("foo▶"))
  }

  it should "allow the user to implicitly accept a search result by performing another action" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 should equal(replState("existing▶").withHistorySearchState(""))

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("f"), state1)
    state2 should equal(replState("foo▶").withHistorySearchState("f", 0, Region(0, 1)))

    val Result(state3, false) = actionHandler.handleAction(BeginningOfLine, state2)
    state3 should equal(replState("foo▶"))
  }

  it should "perform case insensitive searches" in {
    val history = InMemoryHistoryStorage.testHistory("foo", "FOO")
    val actionHandler = IncrementalHistorySearchActionHandler(history)

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 should equal(replState("existing▶").withHistorySearchState(""))

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("f"), state1)
    state2 should equal(replState("FOO▶").withHistorySearchState("f", 0, Region(0, 1)))

    val Result(state3, true) = actionHandler.handleAction(IncrementalHistorySearch, state2)
    state3 should equal(replState("foo▶").withHistorySearchState("f", 1, Region(0, 1)))
  }

  it should "allow user to keep adding to the search, even if no hits" in {
    val history = InMemoryHistoryStorage.testHistory("apple")
    val actionHandler = IncrementalHistorySearchActionHandler(history)

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 should equal(replState("existing▶").withHistorySearchState(""))

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("a"), state1)
    state2 should equal(replState("apple▶").withHistorySearchState("a", 0, Region(0, 1)))

    val Result(state3, true) = actionHandler.handleAction(SelfInsert("a"), state2)
    state3 should equal(replState("▶").withHistorySearchStateAfterLastHit("aa"))
  }

  it should "support starting with an initial search string" in {
    val history = InMemoryHistoryStorage.testHistory("apple")
    val actionHandler = IncrementalHistorySearchActionHandler(history)

    val state0 = replState("app▶")

    val state1 = actionHandler.beginIncrementalSearchFromLine(state0)
    state1 should equal(replState("apple▶").withHistorySearchState("app", 0, Region(0, 3)))
  }

  implicit class RichReplState(state: ReplState) {

    def withHistorySearchState(searchString: String, resultIndex: Int, matchRegion: Region): ReplState =
      state.copy(historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, Hit(resultIndex, matchRegion))))

    def withHistorySearchState(searchString: String): ReplState =
      state.copy(historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, BeforeFirstHit)))

    def withHistorySearchStateAfterLastHit(searchString: String): ReplState =
      state.copy(historySearchStateOpt = Some(IncrementalHistorySearchState(searchString, AfterLastHit)))

  }

  private def replState(s: String) = ReplState(lineBuffer = parseLineBuffer(s))

  private def makeActionHandler: IncrementalHistorySearchActionHandler = {
    val history = InMemoryHistoryStorage.testHistory("foo", "bar", "baz")
    IncrementalHistorySearchActionHandler(history)
  }
}
