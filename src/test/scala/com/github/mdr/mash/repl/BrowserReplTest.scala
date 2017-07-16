package com.github.mdr.mash.repl

class BrowserReplTest extends AbstractReplTest {

  "2D table browser" should "allow column selection" in {
    val twoDBrowser =
      makeRepl()
        .input("view.browser [{ a: 1, b: 2 }, { a: 3, b: 4 }]").acceptLine()
        .affirmInTwoDBrowser
    twoDBrowser.currentRow should equal(0)
    twoDBrowser.currentColumnOpt should equal(None)
    twoDBrowser.nextColumn().currentColumnOpt should equal(Some(0))
    twoDBrowser.unfocusColumn().currentColumnOpt should equal(None)
    twoDBrowser.previousColumn().currentColumnOpt should equal(Some(2))
  }

  "Browser" should "allow moving backwards and forwards through parent item list" in {
    val browser =
      makeRepl()
        .input("view.browser [{ a: 1, b: 2 }, { a: 3, b: 4 }, { a: 5, b: 6 }]")
        .acceptLine()
        .affirmInTwoDBrowser
        .focus()
        .affirmInSingleObjectBrowser
    browser.rows should equal(Seq("a" -> "1", "b" -> "2"))
    browser.path should equal("r0[0]")

    browser.nextParentItem()
    browser.rows should equal(Seq("a" -> "3", "b" -> "4"))
    browser.path should equal("r0[1]")

    browser.nextParentItem()
    browser.path should equal("r0[2]")

    browser.nextParentItem()
    browser.path should equal("r0[0]")

    browser.previousParentItem()
    browser.path should equal("r0[2]")

    browser.previousParentItem()
    browser.path should equal("r0[1]")

    browser.back().affirmInTwoDBrowser.path should equal("r0")
  }


  "Browser" should "allow moving backwards and forwards through parent fields" in {
    val browser =
      makeRepl()
        .input("view.browser { first: { a: 1, b: 2 }, 'second-item': { a: 3, b: 4 } }")
        .acceptLine()
        .affirmInTwoDBrowser
        .focus()
        .affirmInSingleObjectBrowser
    browser.rows should equal(Seq("a" -> "1", "b" -> "2"))
    browser.path should equal("r0.first")

    browser.nextParentItem()
    browser.rows should equal(Seq("a" -> "3", "b" -> "4"))
    browser.path should equal("r0['second-item']")

    browser.previousParentItem()
    browser.path should equal("r0.first")
  }

  "Browser" should "allow navigation to a follow-on expression" in {
    var browser =
      makeRepl()
        .input("view.browser [{ a: 1, b: 2 }, { a: 3, b: 4 }]")
        .acceptLine()
        .affirmInTwoDBrowser
        .beginExpression()
        .input(" | reverse")
        .acceptLine()
        .affirmInTwoDBrowser
    browser.path should equal("r0 | reverse")
    browser.rows should equal(Seq(Seq("0", "3", "4"), Seq("1", "1", "2")))

    browser = browser.back().affirmInTwoDBrowser
    browser.path should equal("r0")
    browser.rows should equal(Seq(Seq("0", "1", "2"), Seq("1", "3", "4")))
  }

  "Browser" should "allow replacement expression" in {
    val browser =
      makeRepl()
        .input("view.browser [{ a: 1, b: 2 }]")
        .acceptLine()
        .affirmInTwoDBrowser
        .beginExpression()
        .backwardKillLine()
        .input("[{ a: 3, b: 4 }]")
        .acceptLine()
        .affirmInTwoDBrowser
    browser.path should equal("[{ a: 3, b: 4 }]")
    browser.rows should equal(Seq(Seq("0", "3", "4")))
  }

}