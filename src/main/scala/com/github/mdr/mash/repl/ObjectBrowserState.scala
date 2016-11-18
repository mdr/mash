package com.github.mdr.mash.repl

trait BrowserState {

  def path: String

}

case class ObjectBrowserState(browserStates: List[BrowserState]) {

  def replaceCurrentState(newState: BrowserState): ObjectBrowserState =
    copy(browserStates = newState :: browserStates.tail)

  def pushNewState(newState: BrowserState): ObjectBrowserState =
    copy(browserStates = newState :: browserStates)

  def pop: ObjectBrowserState =
    copy(browserStates = browserStates.tail)

  def browserState = browserStates.head
}
