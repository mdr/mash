package com.github.mdr.mash.os

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import com.github.mdr.mash.os.linux.GlobHelper

@RunWith(classOf[JUnitRunner])
class GlobHelperTest extends FlatSpec with Matchers {

  "Finding the start directory for a glob" should "work" in {
    GlobHelper.globStart("/etc/*") should equal(Paths.get("/etc/"))
    GlobHelper.globStart("/*") should equal(Paths.get("/"))
    GlobHelper.globStart("foo/bar/*.java") should equal(Paths.get("foo/bar"))
    GlobHelper.globStart("foo/*.java") should equal(Paths.get("foo/"))
    GlobHelper.globStart("*.java") should equal(Paths.get(""))
  }

}