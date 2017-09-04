package com.github.mdr.mash.view.render

import com.github.mdr.mash.assist.{ Assistable, AssistanceState }
import com.github.mdr.mash.ns.collections.ReverseFunction
import com.github.mdr.mash.terminal.DummyTerminal.SufficientlyLargeTerminalSize

class AssistanceRendererTest extends RendererTest {

  val assistanceState = AssistanceState(Assistable.Function(ReverseFunction))

  "Assistance" should "be rendered as large as needed, given sufficient space" in {
    val lines = AssistanceRenderer.render(assistanceState, SufficientlyLargeTerminalSize).map(getText)
    lines shouldEqual Seq(
      "┌─ reverse ────────────────────────┐",
      "│ Reverse a List, String or Object │",
      "│ reverse <sequence>               │",
      "└──────────────────────────────────┘")
  }

  it should "be rendered with appropriate content truncation if there is insufficient space" in {
    val terminalSize = SufficientlyLargeTerminalSize.withColumns(10)
    val lines = AssistanceRenderer.render(assistanceState, terminalSize).map(getText)
    lines shouldEqual Seq(
      "┌─ rev… ─┐",
      "│ Rever… │",
      "│ rever… │",
      "└────────┘")
  }

  it should "render without error down to a minimum size" in {
    val terminalSize = SufficientlyLargeTerminalSize.withColumns(0)
    val lines = AssistanceRenderer.render(assistanceState, terminalSize).map(getText)
    lines shouldEqual Seq(
      "┌─  ─┐",
      "│ R… │",
      "│ r… │",
      "└────┘")
  }

}
