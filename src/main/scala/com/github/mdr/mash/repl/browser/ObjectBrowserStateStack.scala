package com.github.mdr.mash.repl.browser

case class ObjectBrowserStateStack(browserStates: List[BrowserState]) {
  require(browserStates.nonEmpty)

  def parentState: Option[BrowserState] = browserStates.tail.headOption

  def replaceCurrentState(newState: BrowserState): ObjectBrowserStateStack =
    copy(browserStates = newState :: browserStates.tail)

  def pushNewState(newState: BrowserState): ObjectBrowserStateStack =
    copy(browserStates = newState :: browserStates)

  def pop: Option[ObjectBrowserStateStack] =
    if (browserStates.size == 1)
      None
    else
      Some(copy(browserStates = browserStates.tail))

  def headState: BrowserState = browserStates.head

}
