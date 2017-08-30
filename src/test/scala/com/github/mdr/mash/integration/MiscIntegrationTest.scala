package com.github.mdr.mash.integration

import com.github.mdr.mash.Config
import com.github.mdr.mash.assist.{ Assistable, AssistanceState }
import com.github.mdr.mash.ns.collections.ReverseFunction
import com.github.mdr.mash.os.MockFileSystem
import com.github.mdr.mash.repl.LineBufferTestHelper._
import com.github.mdr.mash.repl.completions.BrowserCompletionState
import com.github.mdr.mash.runtime._

class MiscIntegrationTest extends AbstractIntegrationTest {

  "Repl" should "work" in {
    makeRepl()
      .input("1")
      .enter()
      .lastValue shouldEqual MashNumber(1)
  }

  "Single tab" should "complete a unique completion" in {
    val repl = makeRepl()
    repl
      .input("whereNo")
      .complete()
    repl.text shouldEqual "whereNot"
    repl.lineBuffer shouldEqual lineBuffer("whereNot▶")
  }

  "Two tabs" should "enter completions browsing mode" in {
    val repl = makeRepl()
    repl.input("where").complete().complete()
    val Some(_: BrowserCompletionState) = repl.state.completionStateOpt
  }

  "Completion bug after a hyphen" should "not happen" in {
    val repl = makeRepl()
    repl.input("ls -42 # foo").left(8)
    repl.lineBuffer shouldEqual lineBuffer("ls -▶42 # foo")

    repl.complete()

    repl.lineBuffer shouldEqual lineBuffer("ls -▶42 # foo")
  }

  "History" should "not have a bug if you attempt to go forwards in history past the current" in {
    val repl = makeRepl()
    repl.input("1").enter()
    repl.input("2").enter()
    repl.text shouldEqual ""
    repl.up().text shouldEqual "2"
    repl.down().text shouldEqual ""
    repl.down().text shouldEqual ""
    repl.up().text shouldEqual "2"
    repl.up().text shouldEqual "1"
  }

  "History" should "reset if an old line is edited" in {
    val repl = makeRepl()
      .input("command1").enter()
      .input("command2").enter()
      .input("command3").enter()
      .up()
      .up()
    repl.text shouldEqual "command2"
    repl.down().text shouldEqual "command3"
    repl.up().backspace().text shouldEqual "command"

    repl.down()

    repl.text shouldEqual "command"
  }

  "Up" should "only move the cursor in a multiline editor" in {
    val repl = makeRepl().input("{").enter().input("}")
    repl.up().up()
    repl.text shouldEqual "{\n}"
  }

  "Down" should "only move the cursor in a multiline editor" in {
    val repl = makeRepl().input("{").enter().input("}").enter()
    repl.input("command2").enter()
    repl.up().up().end()
    repl.text shouldEqual "{\n}"
    repl.down()
    repl.text shouldEqual "{\n}"
  }

  "History" should "let you navigate past a multiline entry" in {
    val repl = makeRepl().input("command1").enter()
    repl.input("{").enter().input("}").enter()
    repl.input("command3").enter()
    repl.up()
    repl.up()
    repl.text shouldEqual "{\n}"
    repl.up()
    repl.text shouldEqual "command1"
    repl.down()
    repl.text shouldEqual "{\n}"
    repl.down()
    repl.text shouldEqual "command3"
  }

  "Toggling quotes" should "enclose adjacent string in quotes if unquoted, or remove them if quoted" in {
    val repl = makeRepl()
    repl.input("foo")
    repl.toggleQuote().text shouldEqual """"foo""""
    repl.toggleQuote().text shouldEqual "foo"
  }

  "Delete" should "work at the first character" in {
    val repl = makeRepl()

    repl.input("123").left(3).delete()

    repl.lineBuffer shouldEqual lineBuffer("▶23")
  }

  "Repl" should "respect bare words setting" in {
    val repl = makeRepl()
    repl.input(s"config.${Config.Language.BareWords} = true").enter()
    repl.input("foo").enter()
    repl.lastValue shouldEqual MashString("foo")

    repl.input(s"config.${Config.Language.BareWords} = false").enter()
    repl.input("foo").enter()
    repl.lastValue shouldEqual MashBoolean.False /* Repl should have emitted an error */
  }

  "Type inference loop bug" should "not happen" in {
    makeRepl()
      .input("a => a").enter()
      .complete() // previously blew up here
  }

  "Local variables" should "not collide with global" in {
    makeRepl()
      .input("a = 0").enter()
      .input("def setA n = { a = n }").enter()
      .input("setA 42").enter()
      .input("a").enter()
      .lastValue shouldEqual MashNumber(0)
  }

  "Completing dotfiles" should "not have a bug where the original input is truncated" in {
    val repl = makeRepl(MockFileSystem.of("/.dotfiles/.bashrc"))
    repl.input(""""/.dotfiles/".""").complete()
    repl.text shouldEqual """"/.dotfiles/."""" // bug was it was "."
  }

  "Multiline editing" should "be supported" in {
    val repl = makeRepl()
    repl
      .input("{").enter()
      .input("  42").enter()
      .input("}").enter()
    repl.lastValue shouldEqual MashNumber(42)
  }

  "Type inferencer" should "handle previously-defined user-defined nullary functions" in {
    makeRepl()
      .input("foo = { bar: => { baz: 100 } }").enter()
      .input("foo.bar.ba").complete()
      .text shouldEqual "foo.bar.baz"
  }

  "Incremental history search" should "find results matching case-insensitively" in {
    val repl =
      makeRepl()
        .input("FOO = 100").enter()
        .input("foobar = 42").enter()
        .incrementalHistorySearch()
        .input("FOO")
    repl.text shouldEqual "foobar = 42"
    repl.incrementalHistorySearch()
    repl.text shouldEqual "FOO = 100"
  }

  "Incremental history search" should "support starting incremental search from a partial line" in {
    val repl =
      makeRepl()
        .input("FOO = 100").enter()
        .input("foobar = 42").enter()
        .input("FOO")
        .up()
    repl.text shouldEqual "foobar = 42"
    repl.up()
    repl.text shouldEqual "FOO = 100"
  }

  "Inserting last argument" should "be supported" in {
    val repl =
      makeRepl()
        .input("sort 'arg1'").enter()
        .input("sort 'arg2'").enter()
        .input("sort ")
        .insertLastArgument()
    repl.text shouldEqual "sort 'arg2'"
    repl.insertLastArgument()
    repl.text shouldEqual "sort 'arg1'"
    repl.insertLastArgument()
    repl.text shouldEqual "sort 'arg1'"
  }

  "Invocation assistance" should "work" in {
    val repl =
      makeRepl()
        .input("reverse")
        .assistInvocation()
    repl.state.assistanceStateOpt shouldBe Some(
      AssistanceState(Assistable.Function(ReverseFunction)))

    repl.assistInvocation()

    repl.state.assistanceStateOpt shouldBe None
  }

  "Invocation assistance" should "work in expression browser" in {
    val repl =
      makeRepl()
        .input("view.browser [{ foo: 42 }]")
        .enter()
        .affirmInTwoDBrowser
        .beginExpression()
        .input(" | reverse")
        .assistInvocation()

    def assistanceStateOpt = repl.state.objectBrowserStateStackOpt.get.headState.expressionStateOpt.get.assistanceStateOpt

    assistanceStateOpt shouldBe Some(
      AssistanceState(Assistable.Function(ReverseFunction)))

    repl.assistInvocation()

    assistanceStateOpt shouldBe None
  }

  "Inlining expression" should "work" in {
    makeRepl()
      .input("1 + 2 * 3")
      .inline()
      .text shouldEqual "7"
  }

  "Expanding and unexpanding selection" should "work" in {
    val repl = makeRepl()
      .input("1 + 2 * 3")
      .left(2)
    repl.lineBuffer shouldEqual lineBuffer("1 + 2 *▶ 3")
    repl.expandSelection()
    repl.lineBuffer shouldEqual lineBuffer("1 + ▷2 * 3▶")
    repl.expandSelection()
    repl.lineBuffer shouldEqual lineBuffer("▷1 + 2 * 3▶")
    repl.unexpandSelection()
    repl.lineBuffer shouldEqual lineBuffer("1 + ▷2 * 3▶")
    repl.unexpandSelection()
    repl.lineBuffer shouldEqual lineBuffer("1 + 2 *▶ 3")
    repl.unexpandSelection()
    repl.lineBuffer shouldEqual lineBuffer("1 + 2 *▶ 3")
  }

  "Cutting selection and pasting" should "work" in {
    makeRepl()
      .input("1 + 2 * 3")
      .left(2)
      .expandSelection()
      .backwardKillWord()
      .paste()
      .input(" + ")
      .paste()
      .lineBuffer shouldEqual lineBuffer("1 + 2 * 3 + 2 * 3▶")
  }

  "Copy and paste" should "work" in {
    makeRepl()
      .input("1 + 1")
      .left(4, extendSelection = true)
      .copy()
      .end()
      .paste()
      .lineBuffer shouldEqual lineBuffer("1 + 1 + 1▶")
  }

  "Abandoning a command" should "work" in {
    val repl = makeRepl()
      .input("{").enter()
      .input("  def foo = 42")
      .quit()

    repl.lineBuffer shouldEqual lineBuffer("▶")
    repl.state.commandNumber shouldEqual 0
  }

  "Inserting a newline" should "work" in {
    val repl = makeRepl()
      .input("def foo =")
      .newline()
    repl.lineBuffer shouldEqual lineBuffer(
      """def foo =
        |▶""".stripMargin)
  }

  "Undo / redo" should "work" in {
    val repl = makeRepl()
      .input("a")
      .input("b")
      .input("c")
      .backwardKillWord()
    repl.lineBuffer shouldEqual lineBuffer("▶")

    repl.undo()
    repl.lineBuffer shouldEqual lineBuffer("abc▶")
    repl.undo()
    repl.lineBuffer shouldEqual lineBuffer("ab▶")
    repl.undo()
    repl.lineBuffer shouldEqual lineBuffer("a▶")
    repl.undo()
    repl.lineBuffer shouldEqual lineBuffer("▶")
    repl.undo()
    repl.lineBuffer shouldEqual lineBuffer("▶")

    repl.redo()
    repl.lineBuffer shouldEqual lineBuffer("a▶")
    repl.redo()
    repl.lineBuffer shouldEqual lineBuffer("ab▶")
    repl.redo()
    repl.lineBuffer shouldEqual lineBuffer("abc▶")
    repl.redo()
    repl.lineBuffer shouldEqual lineBuffer("▶")
    repl.redo()
    repl.lineBuffer shouldEqual lineBuffer("▶")
  }

}
