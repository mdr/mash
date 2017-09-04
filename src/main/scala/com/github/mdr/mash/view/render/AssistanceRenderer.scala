package com.github.mdr.mash.view.render

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.utils.Dimensions

object AssistanceRenderer {

  def render(assistanceState: AssistanceState, terminalSize: Dimensions): Seq[Line] = {
    val AssistanceState(assistable) = assistanceState
    val boxContent = AssistanceContentGenerator.getAssistanceState(assistable)
    BoxRenderer.render(boxContent, terminalSize)
  }

}
