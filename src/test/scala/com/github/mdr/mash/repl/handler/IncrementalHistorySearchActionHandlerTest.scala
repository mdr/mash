package com.github.mdr.mash.repl.handler

import java.nio.file.{ Path, Paths }

import com.github.mdr.mash.WorkingDirectoryStack
import com.github.mdr.mash.os.{ CurrentDirectoryManager, MockFileSystem }
import com.github.mdr.mash.repl.IncrementalHistorySearchActions.{ FirstHit, LastHit, ToggleCurrentDirOnly }
import com.github.mdr.mash.repl.IncrementalHistorySearchState.{ BeforeFirstHit, Hit, NoHits }
import com.github.mdr.mash.repl.LineBufferTestHelper._
import com.github.mdr.mash.repl.NormalActions._
import com.github.mdr.mash.repl.handler.IncrementalHistorySearchActionHandler.Result
import com.github.mdr.mash.repl.history.InMemoryHistoryStorage
import com.github.mdr.mash.repl.{ IncrementalHistorySearchState, ReplState }
import com.github.mdr.mash.utils.{ MonotonicallyTickingClock, Region }
import org.scalatest.{ FlatSpec, Matchers }

class IncrementalHistorySearchActionHandlerTest extends FlatSpec with Matchers {

  "Incrementally searching history" should "allow cycling through all matches in history, starting with most recent" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 shouldEqual replState("existing▶").withHistorySearchStateBeforeFirstHit("", "existing")

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("b"), state1)
    state2.fixTime shouldEqual replState("baz▶").withHistorySearchState("b", 0, Region(0, 1))

    val Result(state3, true) = actionHandler.handleAction(IncrementalHistorySearch, state2)
    state3.fixTime shouldEqual replState("bar▶").withHistorySearchState("b", 1, Region(0, 1))

    val Result(state4, true) = actionHandler.handleAction(IncrementalHistorySearch, state3)
    state4.fixTime shouldEqual state3.fixTime
  }

  it should "allow characters to be deleted from the search" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 shouldEqual replState("existing▶").withHistorySearchStateBeforeFirstHit("", "existing")

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("a"), state1)
    state2.fixTime shouldEqual replState("baz▶").withHistorySearchState("a", 0, Region(1, 1))

    val Result(state3, true) = actionHandler.handleAction(SelfInsert("r"), state2)
    state3.fixTime shouldEqual replState("bar▶").withHistorySearchState("ar", 0, Region(1, 2))

    val Result(state4, true) = actionHandler.handleAction(BackwardDeleteChar, state3)
    state4.fixTime shouldEqual replState("baz▶").withHistorySearchState("a", 0, Region(1, 1))

    val Result(state5, true) = actionHandler.handleAction(BackwardDeleteChar, state4)
    state5 shouldEqual replState("▶").withHistorySearchStateBeforeFirstHit("", "existing")

    val Result(state6, true) = actionHandler.handleAction(BackwardDeleteChar, state5)
    state6 shouldEqual replState("▶")
  }

  it should "allow the user to accept a search result and exit search" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 shouldEqual replState("existing▶").withHistorySearchStateBeforeFirstHit("", "existing")

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("f"), state1)
    state2.fixTime shouldEqual replState("foo▶").withHistorySearchState("f", 0, Region(0, 1))

    val Result(state3, true) = actionHandler.handleAction(Enter, state2)
    state3 shouldEqual replState("foo▶")
  }

  it should "allow the user to implicitly accept a search result by performing another action" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 shouldEqual replState("existing▶").withHistorySearchStateBeforeFirstHit("", "existing")

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("f"), state1)
    state2.fixTime shouldEqual replState("foo▶").withHistorySearchState("f", 0, Region(0, 1))

    val Result(state3, false) = actionHandler.handleAction(BeginningOfLine, state2)
    state3 shouldEqual replState("foo▶")
  }

  it should "perform case insensitive searches" in {
    val history = InMemoryHistoryStorage.testHistory("foo", "FOO")
    val actionHandler = IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager)

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 shouldEqual replState("existing▶").withHistorySearchStateBeforeFirstHit("", "existing")

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("f"), state1)
    state2.fixTime shouldEqual replState("FOO▶").withHistorySearchState("f", 0, Region(0, 1))

    val Result(state3, true) = actionHandler.handleAction(IncrementalHistorySearch, state2)
    state3.fixTime shouldEqual replState("foo▶").withHistorySearchState("f", 1, Region(0, 1))
  }

  it should "allow user to keep adding to the search, even if no hits" in {
    val history = InMemoryHistoryStorage.testHistory("apple")
    val actionHandler = IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager)

    val state0 = replState("existing▶")

    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    state1 shouldEqual replState("existing▶").withHistorySearchStateBeforeFirstHit(searchString = "", "existing")

    val Result(state2, true) = actionHandler.handleAction(SelfInsert("a"), state1)
    state2.fixTime shouldEqual replState("apple▶").withHistorySearchState("a", 0, Region(0, 1))

    val Result(state3, true) = actionHandler.handleAction(SelfInsert("a"), state2)
    state3 shouldEqual replState("▶").withHistorySearchStateWithNoHits("aa")
  }

  it should "support starting with an initial search string" in {
    val history = InMemoryHistoryStorage.testHistory("apple")
    val actionHandler = IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager)

    val state0 = replState("app▶")

    val state1 = actionHandler.beginIncrementalSearchFromLine(state0)
    state1.fixTime shouldEqual replState("apple▶").withHistorySearchState("app", 0, Region(0, 3), "app")
  }

  it should "allow the user to jump to the first or last hit" in {
    val actionHandler = makeActionHandler

    val state0 = replState("existing▶")
    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    val Result(state2, true) = actionHandler.handleAction(LastHit, state1)

    state2.fixTime shouldEqual replState("foo▶").withHistorySearchState("", 2, Region(0, 0))

    val Result(state3, true) = actionHandler.handleAction(FirstHit, state2)
    state3.fixTime shouldEqual replState("baz▶").withHistorySearchState("", 0, Region(0, 0))
  }

  it should "allow the user to restrict matches to the current working directory" in {
    val appleDir = Paths.get("/appleDir")
    val ballDir = Paths.get("/ballDir")
    val history = InMemoryHistoryStorage.testHistoryWithPaths("apple" → appleDir, "ball" → ballDir)
    val fileSystem = MockFileSystem.of(appleDir.toString)
    fileSystem.chdir(appleDir)
    val actionHandler = IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager)
    val state0 = replState("existing▶")
    val state1 = actionHandler.beginFreshIncrementalSearch(state0)
    val Result(state2, true) = actionHandler.handleAction(ToggleCurrentDirOnly, state1)

    state2.fixTime shouldEqual replState("apple▶").withHistorySearchState("", 0, Region(0, 0), workingDir = appleDir, currentDirOnly = true)

    val Result(state3, true) = actionHandler.handleAction(ToggleCurrentDirOnly, state2)

    state3.fixTime shouldEqual replState("ball▶").withHistorySearchState("", 0, Region(0, 0), workingDir = ballDir, currentDirOnly = false)
  }

  implicit class RichReplState(state: ReplState) {

    def withHistorySearchState(searchString: String,
                               resultIndex: Int,
                               matchRegion: Region,
                               originalLineBuffer: String = "existing",
                               workingDir: Path = InMemoryHistoryStorage.WorkingDirectory,
                               currentDirOnly: Boolean = false): ReplState = {
      val hit = Hit(resultIndex, matchRegion, testTime, workingDir)
      state.copy(incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState(searchString, originalLineBuffer, hit, currentDirOnly)))
    }

    def withHistorySearchStateBeforeFirstHit(searchString: String, existingLineBuffer: String): ReplState =
      state.copy(incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState("", existingLineBuffer, BeforeFirstHit)))

    def withHistorySearchStateWithNoHits(searchString: String): ReplState =
      state.copy(incrementalHistorySearchStateOpt = Some(IncrementalHistorySearchState(searchString, "existing", NoHits)))

    def fixTime: ReplState =
      state.copy(incrementalHistorySearchStateOpt = state.incrementalHistorySearchStateOpt.map(s ⇒
        s.copy(hitStatus = s.hitStatus match {
          case hit: Hit ⇒ hit.copy(timestamp = testTime)
          case other    ⇒ other
        })))
  }

  private val testTime = MonotonicallyTickingClock.instant()

  private def replState(s: String) = ReplState(lineBuffer = lineBuffer(s))

  private def makeActionHandler: IncrementalHistorySearchActionHandler = {
    val history = InMemoryHistoryStorage.testHistory("foo", "bar", "baz")
    IncrementalHistorySearchActionHandler(history, fileSystem, currentDirectoryManager)
  }

  private def currentDirectoryManager =  CurrentDirectoryManager(fileSystem, new WorkingDirectoryStack(InMemoryHistoryStorage.WorkingDirectory))
  private def fileSystem = new MockFileSystem

}