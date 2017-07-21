package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.repl.LineBufferTestHelper.parseLineBuffer
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.repl.handler.InsertLastArgHandler._
import com.github.mdr.mash.utils.Region
import org.scalatest.{ FlatSpec, Matchers }

class InsertLastArgHandlerTest extends FlatSpec with Matchers {

  "InsertLastArgHandler" should "work" in {
    val argSource = TestArgumentSource("1", "22", "3")
    val state0 = replState("foo ▶")

    val state1 = handleInsertLastArg(argSource, state0)
    state1 should equal(replState("foo 1▶").withInsertLastArgState(0, Region(4, 1)))

    val state2 = handleInsertLastArg(argSource, state1)
    state2 should equal(replState("foo 22▶").withInsertLastArgState(1, Region(4, 2)))

    val state3 = handleInsertLastArg(argSource, state2)
    state3 should equal(replState("foo 3▶").withInsertLastArgState(2, Region(4, 1)))

    val state4 = handleInsertLastArg(argSource, state3)
    state4 should equal(state3)
  }


  case class TestArgumentSource(items: String*) extends HistoricalArgumentSource {

    override def getHistoricalArguments(argIndex: Int): Option[String] = items.lift(argIndex)

  }

  implicit class RichReplState(state: ReplState) {

    def withInsertLastArgState(argIndex: Int, region: Region): ReplState =
      state.copy(insertLastArgStateOpt = Some(InsertLastArgState(argIndex, region)))

  }

  private def replState(s: String) = ReplState(lineBuffer = parseLineBuffer(s))

}
