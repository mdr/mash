package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.printer.model.TwoDTableModelCreator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.terminal.DummyTerminal
import org.scalatest.{ FlatSpec, Matchers }

class TwoDTableBrowserStateTest extends FlatSpec with Matchers {

  "A 2D table" should "support moving to the next column" in {
    var state = initBrowser(
      """[
           { a: 1, b: 2 },
           { a: 3, b: 4 }
         ]""")
    state.currentColumnOpt should equal(None)

    state = state.nextColumn
    state.currentColumnOpt should equal(Some(0))

    state = state.nextColumn
    state.currentColumnOpt should equal(Some(1))

    state = state.nextColumn
    state.currentColumnOpt should equal(Some(2))

    state = state.nextColumn
    state.currentColumnOpt should equal(Some(0))
  }

  it should "support moving to the previous column" in {
    var state = initBrowser(
      """[
           { a: 1, b: 2 },
           { a: 3, b: 4 }
         ]""")
    state.currentColumnOpt should equal(None)

    state = state.previousColumn
    state.currentColumnOpt should equal(Some(2))

    state = state.previousColumn
    state.currentColumnOpt should equal(Some(1))

    state = state.previousColumn
    state.currentColumnOpt should equal(Some(0))

    state = state.previousColumn
    state.currentColumnOpt should equal(Some(2))
  }

  def initBrowser(s: String): TwoDTableBrowserState = {
    val tableValue = TestEvaluator.evaluate(s)
    val creator = new TwoDTableModelCreator(DummyTerminal().size, viewConfig = ViewConfig())
    val model = creator.create(tableValue)
    new TwoDTableBrowserState(model, path = "result")
  }
}
