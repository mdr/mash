package com.github.mdr.mash.completions

import java.nio.file.Paths
import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.MockFileObject
import com.github.mdr.mash.os.MockFileObject._
import com.github.mdr.mash.os.MockFileSystem
import com.github.mdr.mash.os.MockEnvironmentInteractions

class PathCompleterTest extends FlatSpec with Matchers {

  "Files starting with the given prefix" should "be found" in {
    withFileSystem("file.txt" -> File()).getCompletions("file") should equal(Seq("file.txt"))
  }

  "Completing a directory" should "end results with '/'" in {
    withFileSystem("dir" -> Directory()).getCompletions("di") should equal(Seq("dir/"))
  }

  "Multiple results" should "be returned if they match the given prefix" in {
    withFileSystem(
      "foo1" -> Directory(),
      "foo2" -> File(),
      "foo3" -> File(),
      "bar" -> File()).getCompletions("foo") should equal(Seq("foo1/", "foo2", "foo3"))
  }

  "Absolute paths" should "be completed" in {
    withFileSystem("root" -> File()).getCompletions("/roo") should equal(Seq("/root"))
  }

  "Completing relative paths with directory segments" should "return paths with those segments" in {
    withFileSystem(
      "dir" -> Directory(
        "file" -> File())).getCompletions("dir/f") should equal(Seq("dir/file"))
  }

  "Completing absolute paths with directory segments" should "return paths with those segments" in {
    withFileSystem(
      "dir" -> Directory(
        "file" -> File())).getCompletions("/dir/f") should equal(Seq("/dir/file"))
  }

  "All files and dirs except those starting with '.'" should "be returned if the prefix is empty" in {
    withFileSystem(
      "dir" -> Directory(),
      "file" -> File(),
      ".dotfile" -> File()).getCompletions("") should equal(Seq("dir/", "file"))
  }

  "Dotfiles" should "should be ignored if the prefix doesn't start with '.'" in {
    withFileSystem(
      "file" -> File(),
      ".dotfile" -> File()).getCompletions("/") should equal(Seq("/file"))
  }

  "Dotfiles" should "be included if the prefix starts with '.'" in {
    withFileSystem(
      ".dotfile" -> File()).getCompletions(".dot") should equal(Seq(".dotfile"))
  }

  "The '..' special directory" should "be completable" in {
    withFileSystem().getCompletions("..") should equal(Seq("../"))
  }

  "The '.' special directory" should "be completable" in {
    withFileSystem().getCompletions(".") should equal(Seq("./", "../"))
  }

  "The '.' and '..' special directories" should "be completable with a path prefix" in {
    withFileSystem().getCompletions("/.") should equal(Seq("/./", "/../"))
  }

  "Substring completions" should "work" in {
    implicit val context = TestContext(substring = true)
    withFileSystem(
      "___123__" -> File(),
      "__123___" -> File())(context).getCompletions("123") should equal(Seq("__123___", "___123__"))
  }

  case class TestContext(substring: Boolean = false)

  case class Helper(pathCompleter: PathCompleter, testContext: TestContext = TestContext()) {
    def getCompletionsFull(searchString: String) = pathCompleter.getCompletions(searchString, substring = testContext.substring)
    def getCompletions(searchString: String) = getCompletionsFull(searchString).map(_.path)
  }

  private def withFileSystem(children: (String, MockFileObject)*)(implicit testContext: TestContext = TestContext()): Helper =
    Helper(new PathCompleter(new MockFileSystem(Directory(children: _*)), MockEnvironmentInteractions()), testContext)

}