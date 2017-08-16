package com.github.mdr.mash

import scala.language.implicitConversions
import java.nio.file.{ Path, Paths }

import org.scalatest.{ FlatSpec, Matchers }

class WorkingDirectoryStackTest extends FlatSpec with Matchers {

  implicit def stringToPath(s: String): Path = Paths.get(s)

  "Working directory stack" should "not have a bug" in {
    val stack = new WorkingDirectoryStack("/initial")
    stack.oldDirs shouldEqual Seq()
    stack.push("/foo")
    stack.oldDirs.map(_.toString) shouldEqual Seq("/initial")
    stack.back()
    stack.oldDirs shouldEqual Seq()
    stack.push("/bar")
    stack.oldDirs.map(_.toString) shouldEqual Seq("/initial")
    stack.push("/baz")
    stack.oldDirs.map(_.toString) shouldEqual Seq("/initial", "/bar")
  }

  it should "allow forward/backward navigation" in {
    val stack = new WorkingDirectoryStack("/initial")
    stack.push("/foo")
    stack.back().map(_.toString) shouldEqual Some("/initial")
    stack.forward().map(_.toString) shouldEqual Some("/foo")
    stack.oldDirs.map(_.toString) shouldEqual Seq("/initial")
  }

}
