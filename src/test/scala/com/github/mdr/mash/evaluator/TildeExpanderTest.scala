package com.github.mdr.mash.evaluator

import com.github.mdr.mash.os.MockEnvironmentInteractions
import org.junit.runner.RunWith
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TildeExpanderTest extends FlatSpec with Matchers {

  val tildeExpander = new TildeExpander(MockEnvironmentInteractions("/home/alice"))

  "Tilde expansion" should "expand ~ at the beginning of a string" in {
    tildeExpander.expand("~") should equal("/home/alice")
    tildeExpander.expand("~/.mash") should equal("/home/alice/.mash")
  }

  "Tilde expansion" should "can record whether or not it took place" in {
    tildeExpander.expandOpt("~") should equal(Some("/home/alice"))
    tildeExpander.expandOpt("nope") should equal(None)
    tildeExpander.expandOpt("/home/alice") should equal(None)
  }

  "Retilde-ing a string" should "replace a prefix of the user's home with a ~" in {
    tildeExpander.retilde("/home/alice") should equal("~")
    tildeExpander.retilde("/home/alice/") should equal("~/")
    tildeExpander.retilde("/home/alice/.mash") should equal("~/.mash")
  }

  "Retilde-ing a string" should "return how many characters were lost" in {
    tildeExpander.retildeFull("/home/alice/foo/bar").charsLost should equal("/home/alice".length - 1)
  }
}