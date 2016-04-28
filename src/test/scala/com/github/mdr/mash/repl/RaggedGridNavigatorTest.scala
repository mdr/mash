package com.github.mdr.mash.repl

import org.scalatest._

class RaggedGridNavigatorTest extends FlatSpec with Matchers {

  check("""
    ▼▢
    ▢▢   """,
    """
    ▢▢
    ▣▢   """)

  check("""
    ▼▢  """,
    """
    ▣▢   """)

  check("""
    ▢▢▢▢
    ▢▢▼▢
    ▢▢   """,
    """
    ▢▢▣▢
    ▢▢▢▢
    ▢▢   """)

  check("""
    ▢▢▢▢
    ▢▢▢▢
    ▢▼   """,
    """
    ▢▣▢▢
    ▢▢▢▢
    ▢▢   """)

  private class Context(s: String, navigator: RaggedGridNavigator) {
    def navigatingShouldResultIn(x: Any) {}
  }

  private def check(before: String, after: String) = {
    "RaggedGridNavigator" should s"transform \n$before into \n$after" in {
      val actual = getActual(before)
      val expected = getExpected(after)
      actual should equal(expected)
    }
  }

  private def getExpected(after: String): Int = {
    val lines = after.split("\n").map(_.trim).toSeq.tail
    val columns = lines(0).length
    val row = lines.indexWhere(_ contains "▣")
    val line = lines(row)
    val column = line.indexWhere(_ == '▣')
    row * columns + column
  }

  private def getActual(before: String): Int = {
    val symbols = "▼▶◀▲".toSet
    val lines = before.split("\n").map(_.trim).toSeq.tail
    val total = lines.map(_.length).sum
    val columns = lines(0).length
    val row = lines.indexWhere(_.exists(symbols.contains))
    val line = lines(row)
    val column = line.indexWhere(symbols.contains)
    val pos = row * columns + column
    val navigator = RaggedGridNavigator(total, columns, pos)
    val actual = line(column) match {
      case '▼' ⇒ navigator.down
      case '▶' ⇒ navigator.right
      case '◀' ⇒ navigator.left
      case '▲' ⇒ navigator.up
    }
    actual
  }

}