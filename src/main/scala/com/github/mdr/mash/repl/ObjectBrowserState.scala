package com.github.mdr.mash.repl

import com.github.mdr.mash.runtime.MashValue

trait BrowserState {
  def rawValue: MashValue

  def path: String

  def withPath(newPath: String): BrowserState

}

case class ObjectBrowserState(browserStates: List[BrowserState]) {
  require(browserStates.nonEmpty)

  def replaceCurrentState(newState: BrowserState): ObjectBrowserState =
    copy(browserStates = newState :: browserStates.tail)

  def pushNewState(newState: BrowserState): ObjectBrowserState =
    copy(browserStates = newState :: browserStates)

  def pop: ObjectBrowserState =
    copy(browserStates = browserStates.tail)

  def browserState: BrowserState = browserStates.head

}
