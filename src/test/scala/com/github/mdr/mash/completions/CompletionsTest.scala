package com.github.mdr.mash.completions

import java.nio.file.Paths

import com.github.mdr.mash.evaluator.{ Environment, StandardEnvironment }
import com.github.mdr.mash.os.MockFileObject._
import com.github.mdr.mash.os.{ EnvironmentInteractions, FileSystem, MockEnvironmentInteractions, MockFileSystem }
import com.github.mdr.mash.repl.LineBufferTestHelper
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import org.apache.commons.lang3.SystemUtils
import org.scalatest._

class CompletionsTest extends FlatSpec with Matchers {

  // ▶ denotes the cursor position when completions are requested

  {
    implicit val filesystem = MockFileSystem.of("/foo")

    "▶" shouldContainCompletion "ls"
    "▶" shouldContainCompletion "foo"
  }

  "{ foo: 42 }.fo▶" shouldGiveCompletions "foo"

  "{ bar: 1, baz: 2, buzz: 3 }.ba▶" shouldGiveCompletions ("bar", "baz")

  "{ foo: 1 } | _.▶" shouldContainCompletion "foo"

  "[ { foo: 1 } ] | map (_.▶)" shouldContainCompletion "foo"

  "[ { revert: 1 } ].rever▶" shouldGiveCompletions ("reverse", "revert")

  """ "a string".startsW▶ """ shouldGiveCompletions "startsWith"

  "Completions in member position" should "not include binding completions" in {
    " ''.histor▶ ".completions should not contain "history"
  }

  {
    implicit val filesystem = MockFileSystem.of("/home/alice/file.txt")

    "cd /hom▶" shouldGiveCompletions "/home/"
    "cd /home/al▶" shouldGiveCompletions "/home/alice/"
    """cd "/home"/al▶""" shouldGiveCompletions "/home/alice/"
    "cd(/hom▶" shouldGiveCompletions "/home/"
  }

  {
    implicit val filesystem = new MockFileSystem(Directory(
      "dir" -> Directory(),
      "document.txt" -> File()))

    "cd ▶" shouldGiveCompletions "dir/"
    "cd d▶" shouldGiveCompletions "dir/"
    "cd pw▶" shouldGiveCompletions "pwd"

    "cd(▶" shouldGiveCompletions "dir/"
    "cd(d▶" shouldGiveCompletions "dir/"
    "cd(pw▶" shouldGiveCompletions "pwd"

    "d▶ | cd" shouldGiveCompletions "dir/"
    """cd --directory="d"▶""" shouldGiveCompletions "dir/"
    //    """cd --directory=d▶""" shouldGiveCompletions ("dir/")
  }

  {
    implicit val filesystem = MockFileSystem.of("/spaces in name")
    "readLines spaces▶" shouldGiveCompletions "spaces in name"
  }

  "Completing after a dot after a space" should "prioritise file completions" in {
    implicit val filesystem = MockFileSystem.of("/.dotfile")
    "readLines .▶".completions should contain theSameElementsAs Seq("./", "../", ".dotfile")
  }

  "Completing after a dot when a file with that name exists" should "prioritise file name completion" in {
    implicit val filesystem = MockFileSystem.of("/readLines.txt")
    "readLines.▶".completions should contain theSameElementsAs Seq("readLines.txt")
  }

  ".▶" shouldGiveCompletions ("../", "./")
  "cd .▶" shouldGiveCompletions ("../", "./")
   "cd ..▶" shouldGiveCompletions "../"

  "def foo (n = ls.permiss▶)" shouldGiveCompletions "permissions"
  "(n = ls.permiss▶) => 42" shouldGiveCompletions "permissions"
  "class Foo (n = ls.permiss▶)" shouldGiveCompletions "permissions"

  "def foo (n = ls) = n.permiss▶" shouldGiveCompletions "permissions"
  "(n = ls) => n.permiss▶" shouldGiveCompletions "permissions"
  "class Foo (n = ls) { def foo = n.permiss▶ }" shouldGiveCompletions "permissions"

  {
    implicit val filesystem = MockFileSystem.of("/readme.txt")

    "read▶" shouldGiveCompletions ("readLines", "readme.txt")
    "readme.t▶" shouldGiveCompletions "readme.txt"
    """ "readme.t▶" """ shouldGiveCompletions "readme.txt"
    """ "readme".t▶ """ shouldGiveCompletions "readme.txt"
    "readme.▶" shouldGiveCompletions "readme.txt"
  }

  {
    implicit val filesystem = MockFileSystem.of("/readme.txt")
    implicit val environment = Environment(MashObject.of(Seq("readme" -> MashString("readme"))))

    "readme.▶ # with binding" shouldGiveCompletions "readme.txt"
    "readme.t▶ # with binding" shouldGiveCompletions "readme.txt"
  }

  {
    implicit val filesystem = MockFileSystem.of("/file.txt")

    "readLines ▶" shouldGiveCompletions "file.txt" // no bindings
  }

  {
    implicit val filesystem = MockFileSystem.of("/foo.txt")

    "[{ foo: 42 }] | map fo▶" shouldGiveCompletions "foo"
    "[{ bar: 42 }] | map fo▶" shouldGiveCompletions "foo.txt"
  }

  "{ foo: 42 } | select fo▶" shouldGiveCompletions "foo"
  "[{ foo: 42 }] | select fo▶" shouldGiveCompletions "foo"

  // Flags
  "pwd.children --recur▶" shouldGiveCompletions "--recursive"
  "select --add▶" shouldGiveCompletions "--add"
  "select --▶" shouldGiveCompletions ("--add", "--target")
  "cd -▶" shouldGiveCompletions "--directory"
  "ls -▶" shouldContainCompletion "-a"
  "'dir' | ls -▶" shouldContainCompletion "-a"

  {
    implicit val filesystem = MockFileSystem.of("/-")

    "-▶" shouldGiveCompletions "-"
    "ls -▶" shouldContainCompletion "-"
  }

  {
    implicit val filesystem = MockFileSystem.of("/--")

    "--▶" shouldGiveCompletions "--"
    "ls --▶" shouldContainCompletion "--"
  }

  "[{ foo: 42 }].map fo▶" shouldGiveCompletions "foo"
  "map permiss▶ --sequence=ls" shouldGiveCompletions "permissions"

  "{ foo: 42 }?.fo▶" shouldGiveCompletions "foo"
  "{ foo: 42 }?.▶" shouldContainCompletion "foo"

  "..▶" shouldGiveCompletions "../"

  "[].maxBy.targe▶" shouldGiveCompletions "target"

  {
    implicit val filesystem = new MockFileSystem(Directory(
      "directory" -> Directory()), pwd = Paths.get("/directory"))

    // "../dir▶" shouldGiveCompletions "../directory"
    // TODO: need mock file system to support .. paths to get this to work
  }

  // To get this to work, we need to be able to identify the name corresponding to the nth argument (by syntactical position)
  "map --sequence=ls permiss▶" shouldGiveCompletions "permissions"

  {
    implicit val filesystem = MockFileSystem.of("/dollar$ign")

    """ dollar▶ """ shouldGiveCompletions """dollar`$ign"""
    """ dollar`$i▶ """ shouldGiveCompletions """dollar`$ign"""
    """ dollar`$▶ """ shouldGiveCompletions """dollar`$ign"""
    """ dollar$▶ """ shouldGiveCompletions """dollar`$ign"""
    """ "dollar"▶ """ shouldGiveCompletions """dollar`$ign"""
    """ "dollar`$i"▶ """ shouldGiveCompletions """dollar`$ign"""
    """ "dollar`$"▶ """ shouldGiveCompletions """dollar`$ign"""
    """ "dollar$"▶ """ shouldGiveCompletions """dollar`$ign"""
  }

  {
    implicit val filesystem = MockFileSystem.of("/quotation\"mark")

    """ quotation▶ """ shouldGiveCompletions """quotation`"mark"""
    """ "quotation▶""" shouldGiveCompletions """quotation`"mark"""

    // Would like to get these cases working too:
    //  """ quotation\"mar▶ """ shouldGiveCompletions """quotation\"mark"""
    //  """ quotation"▶ """ shouldGiveCompletions """quotation\"mark"""
    //  """ "quotation\"▶""" shouldGiveCompletions """quotation\"mark"""
    //  """ quotation\"▶ """ shouldGiveCompletions """quotation\"mark"""
    //  """ "quotation▶ """ shouldGiveCompletions """quotation\"mark"""
  }

  // tilde 
  {
    implicit val filesystem = MockFileSystem.of("/home/alice/file.txt")
    implicit val envInteractions = MockEnvironmentInteractions("/home/alice")

    "~/file▶" shouldGiveCompletions "~/file.txt"

    "~▶" shouldGiveCompletions "~/"
  }

  // Inheritance: just one completion
  "pwd.info.toStrin▶" shouldGiveCompletions "toString"

  "{}.toStrin▶" shouldGiveCompletions "toString"

  // Vectorisation: just one completion
  "[4].toStrin▶" shouldGiveCompletions "toString"

  // Completion positions
  "{}.▶toStrin" shouldGiveCompletions "toString"
  "ls ▶--al" shouldGiveCompletions "--all"
  "ls ▶-" shouldContainCompletion "--all"
  "1▶." shouldContainCompletion "days"
  "1▶?." shouldContainCompletion "days"

  {
    implicit val filesystem = MockFileSystem.of("/file.txt")

    """ ("file."▶) """ shouldGiveCompletions "file.txt"
    """ (▶"file.") """ shouldGiveCompletions "file.txt"
    "(/▶" shouldGiveCompletions "/file.txt"
    "(▶" shouldContainCompletion "file.txt"
    "readLines(file.▶" shouldGiveCompletions "file.txt"
    "readLines(▶" shouldContainCompletion "file.txt"
  }

  // """ps | where (_.owner == "roo▶""" shouldGiveCompletions "root"

  "Seq completions" should "not be marked as vectorised" in {
    val Some(completion) = "pwd.children.isEmpt▶".fullCompletions.find(_.displayText == "isEmpty")
    val Some(description) = completion.descriptionOpt
    description should not include "vectorised"
  }

  // Substring completions 

  {
    implicit val filesystem = new MockFileSystem(Directory(
      "abc123xyz" -> File(),
      "foobar123baz" -> File()))

    "123▶" shouldGiveCompletions ("abc123xyz", "foobar123baz")
    "1▶" shouldGiveCompletions ("abc123xyz", "foobar123baz")
  }

  {
    implicit val filesystem = new MockFileSystem(Directory(
      "foo---" -> File(),
      "---foo" -> File()))

    "foo▶" shouldGiveCompletions "foo---"
  }

  {
    implicit val filesystem = MockFileSystem.of("/foo-bar")

    "cd foo-▶" shouldGiveCompletions "foo-bar"
  }

  "{ foobar: 42 }.foo▶ = 42" shouldGiveCompletions "foobar"
  "a = { foobar: 42}.foo▶" shouldGiveCompletions "foobar"

  "kill --signal=H▶" shouldGiveCompletions "HUP"

  {
    implicit val filesystem = MockFileSystem.of("/.git")

    "[{ gitty: 42 }].gi▶" shouldGiveCompletions "gitty"
    "[{ gitty: 42 }].▶" shouldContainCompletion "gitty"
    "[{ gitty: 42 }] .gi▶" shouldGiveCompletions ".git"
  }

  {
    implicit val fileSystem = MockFileSystem.of("/file")

    "ls -r▶" shouldGiveCompletions ()
  }

  "{ foobar: 42 } | .foo▶" shouldGiveCompletions "foobar"

  {
    implicit val fileSystem = MockFileSystem.of("/.dotfiles/.bashrc")

    """ls ".dotfiles/."▶""" shouldGiveCompletions (".dotfiles/../", ".dotfiles/./", ".dotfiles/.bashrc")
    """ls ".dotfiles/".▶""" shouldGiveCompletions (".dotfiles/../", ".dotfiles/./", ".dotfiles/.bashrc")
  }

  if (!SystemUtils.IS_OS_MAC_OSX) {
    """ "${user.fullNam▶} """ shouldGiveCompletions "fullName"
    """ "$user.fullNam▶ """ shouldGiveCompletions "fullName"
  }
  
  " [{ foo: true }] | find fo▶" shouldGiveCompletions "foo"
  
  " { ls.fir▶ : 42 }" shouldGiveCompletions "first"

  "(foo bar => foo.perm▶) --bar=100 --foo=ls.first" shouldGiveCompletions "permissions"

  "def foo = { ls.perm▶" shouldGiveCompletions "permissions"

  // Tracks new bindings:
  "foobar => foob▶" shouldGiveCompletions "foobar"
  "{ foobar } => foob▶)" shouldGiveCompletions "foobar"
  "foobar = 42; foob▶" shouldGiveCompletions "foobar"
  "{ foobar } = { foobar: 42 }; foob▶" shouldGiveCompletions "foobar"
  "{ foobar: { bazzle } } = { foobar: { bazzle: 42 } }; bazz▶" shouldGiveCompletions "bazzle"
  "{ foobar: { bazzle: buzzle } } = { foobar: { bazzle: 42 } }; buzz▶" shouldGiveCompletions "buzzle"
  "def fun foobar = foob▶" shouldGiveCompletions "foobar"
  "foobundle = 42; { foobar = 42 }; foob▶" shouldGiveCompletions "foobundle"
  "[a, b, foobar] = [1, 2, 3]; foob▶" shouldGiveCompletions "foobar"
  "[a, b, foobar] = [1, 2]; foob▶" shouldGiveCompletions "foobar"

  "class Bob { def bob = [].reve▶" shouldGiveCompletions "reverse"
  "class Point x y; Poin▶" shouldGiveCompletions "Point"
  "{ class Rectangle width height { def area = width * height }; Rectangle(3, 4).are▶ }" shouldGiveCompletions "area"
  "{ class Rectangle width height; Rectangle(3, 4).wid▶ }" shouldGiveCompletions "width"
  "class Foo { def foobar = 42 }; Foo.new.▶" shouldContainCompletion "foobar"
  "class Foo { def foobar n = n.▶ }" shouldContainCompletion "toString"
  "class Foo wibble { def foo = wibb▶ }" shouldGiveCompletions "wibble"
  "class Foo { def foo wibble = wibb▶ }" shouldGiveCompletions "wibble"

  {
    implicit val fileSystem = MockFileSystem.of("/.dotfile")

    "42 | .▶" shouldContainCompletion "toString"
    // "ls | .d▶" shouldContainCompletion "delete"
  }

  "Object.▶" shouldContainCompletion "merge"
  "Object.m▶" shouldContainCompletion "merge"
  "Object.p▶" shouldContainCompletion "parent"

  private implicit class RichString(s: String)(
      implicit val fileSystem: FileSystem = new MockFileSystem,
      implicit val envInteractions: EnvironmentInteractions = MockEnvironmentInteractions(),
      implicit val environment: Environment = StandardEnvironment.create) {

    def shouldGiveCompletions(expectedCompletions: String*) {
      val expectedDescription = expectedCompletions.mkString(", ")
      "Completer" should s"offer completions for '$s': $expectedDescription" in {
        completions shouldEqual expectedCompletions
      }
    }

    def shouldContainCompletion(expectedCompletion: String) {
      "Completer" should s"offer completions for '$s' including: $expectedCompletion" in {
        completions should contain(expectedCompletion)
      }
    }

    def shouldNotContainCompletion(expectedCompletion: String) {
      "Completer" should s"offer completions for '$s' without: $expectedCompletion" in {
        completions should not contain expectedCompletion
      }
    }

    def completions: Seq[String] = fullCompletions.map(_.insertText)

    def fullCompletions: Seq[Completion] = {
      val lineBuffer = LineBufferTestHelper.parseLineBuffer(s)
      val completer = new Completer(fileSystem, envInteractions)
      completer.complete(lineBuffer.text, lineBuffer.cursorOffset, environment.bindings, mish = false).map(_.completions).getOrElse(Seq())
    }

  }

}
