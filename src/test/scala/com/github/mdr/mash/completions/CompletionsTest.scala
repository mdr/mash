package com.github.mdr.mash.completions

import org.scalatest._
import com.github.mdr.mash.LineBufferTestHelper
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.MockEnvironmentInteractions
import com.github.mdr.mash.os.MockFileObject._
import com.github.mdr.mash.os.MockFileSystem
import java.nio.file.Paths

class CompletionsTest extends FlatSpec with Matchers {

  // Note: ▶ denotes the cursor position when completions are requested

  {
    implicit val filesystem = MockFileSystem.of("/foo")

    "▶" shouldContainCompletion "ls"
    "▶" shouldContainCompletion "foo"
  }

  "{ foo: 42 }.fo▶" shouldGiveCompletions ("foo")

  "{ bar: 1, baz: 2, buzz: 3 }.ba▶" shouldGiveCompletions ("bar", "baz")

  "{ foo: 1 } | _.▶" shouldContainCompletion ("foo")

  "[ { foo: 1 } ] | map (_.▶)" shouldContainCompletion ("foo")

  "[ { revert: 1 } ].rever▶" shouldGiveCompletions ("reverse", "revert")

  """ "a string".startsW▶ """ shouldGiveCompletions ("startsWith")

  "Completions in member position" should "not include binding completions" in {
    " ''.groupB▶ ".completions should not contain ("groupBy")
  }

  {
    implicit val filesystem = MockFileSystem.of("/home/alice/file.txt")

    "cd /hom▶" shouldGiveCompletions ("/home/")
    "cd /home/al▶" shouldGiveCompletions ("/home/alice/")
    """cd "/home"/al▶""" shouldGiveCompletions ("/home/alice/")
    "cd(/hom▶" shouldGiveCompletions ("/home/")
  }

  {
    implicit val filesystem = new MockFileSystem(Directory(
      "dir" -> Directory(),
      "document.txt" -> File()))

    "cd ▶" shouldGiveCompletions ("dir/")
    "cd d▶" shouldGiveCompletions ("dir/")
    "cd pw▶" shouldGiveCompletions ("pwd")

    "cd(▶" shouldGiveCompletions ("dir/")
    "cd(d▶" shouldGiveCompletions ("dir/")
    "cd(pw▶" shouldGiveCompletions ("pwd")
  }

  {
    implicit val filesystem = MockFileSystem.of("/spaces in name")
    "readLines spaces▶" shouldGiveCompletions ("spaces in name")
  }

  " ''.groupB▶ " shouldNotContainCompletion "groupBy"

  "Completing after a dot after a space" should "prioritise file completions" in {
    implicit val filesystem = MockFileSystem.of("/.dotfile")
    "readLines .▶".completions should contain theSameElementsAs Seq("./", "../", ".dotfile")
  }

  "Completing after a dot when a file with that name exists" should "prioritise member completion" in {
    implicit val filesystem = MockFileSystem.of("/readLines.txt")
    "readLines.▶".completions should contain("toString")
  }

  ".▶" shouldGiveCompletions ("../", "./")

  {
    implicit val filesystem = MockFileSystem.of("/readme.txt")

    "read▶" shouldGiveCompletions ("readLines", "readme.txt")
    "readme.tx▶" shouldGiveCompletions ("readme.txt")

    "readme.▶" shouldGiveCompletions ("readme.txt")
  }

  {
    implicit val filesystem = MockFileSystem.of("/file.txt")

    "readLines ▶" shouldGiveCompletions ("file.txt") // no bindings
  }

  {
    implicit val filesystem = MockFileSystem.of("/foo.txt")

    "[{ foo: 42 }] | map fo▶" shouldGiveCompletions ("foo")
    "[{ bar: 42 }] | map fo▶" shouldGiveCompletions ("foo.txt")
  }

  "{ foo: 42 } | select fo▶" shouldGiveCompletions ("foo")
  "[{ foo: 42 }] | select fo▶" shouldGiveCompletions ("foo")

  // Flags
  "pwd.children --recur▶" shouldGiveCompletions ("--recursive")
  "select --add▶" shouldGiveCompletions ("--add")
  "select --▶" shouldGiveCompletions ("--add", "--target")
  "cd -▶" shouldGiveCompletions ("--directory")
  "ls -▶" shouldContainCompletion "-a"
  "'dir' | ls -▶" shouldContainCompletion "-a"

  {
    implicit val filesystem = MockFileSystem.of("/-")

    "-▶" shouldGiveCompletions ("-")
    "ls -▶" shouldContainCompletion "-"
  }

  {
    implicit val filesystem = MockFileSystem.of("/--")

    "--▶" shouldGiveCompletions ("--")
    "ls --▶" shouldContainCompletion "--"
  }

  "[{ foo: 42 }].map fo▶" shouldGiveCompletions ("foo")
  "map permiss▶ --sequence=ls" shouldGiveCompletions ("permissions")

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
  // This can be done by ParameterModel.bindTypes
  // "map --sequence=ls permiss▶" shouldGiveCompletions ("permissions")

  """ "${user.fullNam▶} """ shouldGiveCompletions "fullName"
  """ "$user.fullNam▶ """ shouldGiveCompletions "fullName"

  {
    implicit val filesystem = MockFileSystem.of("/dollar$ign")

    """ dollar▶ """ shouldGiveCompletions """dollar\$ign"""
    """ dollar\$i▶ """ shouldGiveCompletions """dollar\$ign"""
    """ dollar\$▶ """ shouldGiveCompletions """dollar\$ign"""
    """ dollar$▶ """ shouldGiveCompletions """dollar\$ign"""
    """ "dollar"▶ """ shouldGiveCompletions """dollar\$ign"""
    """ "dollar\$i"▶ """ shouldGiveCompletions """dollar\$ign"""
    """ "dollar\$"▶ """ shouldGiveCompletions """dollar\$ign"""
    """ "dollar$"▶ """ shouldGiveCompletions """dollar\$ign"""
  }

  {
    implicit val filesystem = MockFileSystem.of("/quotation\"mark")

    """ quotation▶ """ shouldGiveCompletions """quotation\"mark"""
    """ "quotation▶""" shouldGiveCompletions """quotation\"mark"""

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
    description should not include ("vectorised")
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

    "foo▶" shouldGiveCompletions ("foo---")
  }

  {
    implicit val filesystem = MockFileSystem.of("/foo-bar")

    "cd foo-▶" shouldGiveCompletions ("foo-bar")
  }

  private implicit class RichString(s: String)(
      implicit val fileSystem: FileSystem = new MockFileSystem,
      implicit val envInteractions: EnvironmentInteractions = MockEnvironmentInteractions()) {

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
        completions should not contain (expectedCompletion)
      }
    }

    def completions: Seq[String] = fullCompletions.map(_.insertText)

    def fullCompletions: Seq[Completion] = {
      val lineBuffer = LineBufferTestHelper.parseLineBuffer(s)
      val env = Environment.create
      val completer = new Completer(fileSystem, envInteractions)
      completer.complete(lineBuffer.text, lineBuffer.cursorPos, env, mish = false).map(_.completions).getOrElse(Seq())
    }

  }

}
