package com.github.mdr.mash.repl.history

import java.nio.file.Paths

import org.scalatest.{ FlatSpec, Matchers }

class HistoryTest extends FlatSpec with Matchers {

  "History" should "stop at the first entry" in {
    val history = makeHistory
    history.record("command1")
    history.record("command2")
    val Some("command2") = history.goBackwards("inprogress")
    val Some("command1") = history.goBackwards("command2")
    val None = history.goBackwards("command1")
  }

  "History" should "remember an in-progress command at the end of history" in {
    val history = makeHistory
    history.record("command1")
    val Some("command1") = history.goBackwards("inprogress")
    val Some("inprogress") = history.goForwards()
  }

  def makeHistory = new HistoryImpl(new InMemoryHistoryStorage)

  implicit class RichHistory(history: History) {

    def record(command: String) =
      history.record(command, commandNumber = 1, workingDirectory = Paths.get(""))

  }

}