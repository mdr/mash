package com.github.mdr.mash.repl

object UndoRedoState {

  val Clean = UndoRedoState()

}

case class UndoRedoState(previousLineBuffers: List[LineBuffer] = List(),
                         undoneLineBuffers: List[LineBuffer] = List()) {

  def push(lineBuffer: LineBuffer): UndoRedoState =
    copy(previousLineBuffers = lineBuffer :: previousLineBuffers, undoneLineBuffers = List())

  def undo(lineBuffer: LineBuffer): Option[(LineBuffer, UndoRedoState)] = previousLineBuffers match {
    case Nil          ⇒ None
    case head :: tail ⇒ Some(head, copy(previousLineBuffers = tail, undoneLineBuffers = lineBuffer :: undoneLineBuffers))
  }

  def redo(lineBuffer: LineBuffer): Option[(LineBuffer, UndoRedoState)] = undoneLineBuffers match {
    case Nil          ⇒ None
    case head :: tail ⇒ Some(head, copy(previousLineBuffers = lineBuffer :: previousLineBuffers, undoneLineBuffers = tail))
  }

}
