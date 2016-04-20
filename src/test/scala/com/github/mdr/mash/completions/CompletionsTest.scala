package com.github.mdr.mash.completions

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.os.MockFileSystem
import com.github.mdr.mash.os.FileSystem
import java.nio.file.Paths
import com.github.mdr.mash.os.MockFileObject
import scala.collection.immutable.ListMap
import com.github.mdr.mash.os.MockEnvironmentInteractions
import com.github.mdr.mash.LineBufferTestHelper

class CompletionsTest extends FlatSpec with Matchers {

  // Note: ▶ denotes the cursor position when completions are requested

  "{ foo: 42 }.fo▶" shouldGiveCompletions ("foo")

  "{ bar: 1, baz: 2, buzz: 3 }.ba▶" shouldGiveCompletions ("bar", "baz")

  "{ foo: 1 } | _.▶" shouldContainCompletion ("foo")

  "[ { foo: 1 } ] | map (_.▶)" shouldContainCompletion ("foo")

  "[ { revert: 1 } ].rever▶" shouldGiveCompletions ("reverse", "revert")

  "\"a string\".startsW▶" shouldGiveCompletions ("startsWith")

  "pwd.children --recur▶" shouldGiveCompletions ("--recursive")

  "Completions in member position" should "not include binding completions" in {
    " ''.groupB▶ ".completions should not contain ("groupBy")
  }

  "cd /hom▶" shouldGiveCompletions ("/home/")
  "cd /home/ma▶" shouldGiveCompletions ("/home/mash/")
  """cd "/home"/ma▶""" shouldGiveCompletions ("/home/mash/")

  "readLines spaces▶" shouldGiveCompletions ("spaces in name")

  " ''.groupB▶ " shouldNotContainCompletion "groupBy"

  "Completing after a dot after a space" should "prioritise file completions" in {
    "readLines .▶".completions shouldEqual (Seq(".mashrc"))
  }

  "Completing after a dot" should "prioritise member completion" in {
    "readLines.▶".completions should contain("toString")
  }

  "read▶" shouldGiveCompletions ("readLines", "readme.txt")
  "readme.tx▶" shouldGiveCompletions ("readme.txt")

  // Not sure about this one yet. With bare words, it arguably should complete the path members
  // "readme.▶" shouldGiveCompletions ("readme.txt")

  "cd ▶" shouldGiveCompletions ("dir/")
  "cd d▶" shouldGiveCompletions ("dir/")
  "cd pw▶" shouldGiveCompletions ("pwd")

  "readLines ▶" shouldNotContainCompletion "ls" // i.e. no bindings

  "[{ foo: 42 }] | map fo▶" shouldGiveCompletions ("foo")
  "[{ bar: 42 }] | map fo▶" shouldGiveCompletions ("foo.txt")

  "{ foo: 42 } | select fo▶" shouldGiveCompletions ("foo")
  "[{ foo: 42 }] | select fo▶" shouldGiveCompletions ("foo")
  "select --add▶" shouldGiveCompletions ("--add")
  "cd -▶" shouldGiveCompletions ("--directory")
  "ls -▶" shouldContainCompletion "-a"

  "[{ foo: 42 }].map fo▶" shouldGiveCompletions ("foo")
  "map permiss▶ --sequence=ls" shouldGiveCompletions ("permissions")

  "{ foo: 42 }?.fo▶" shouldGiveCompletions "foo"
  "{ foo: 42 }?.▶" shouldContainCompletion "foo"

  "..▶" shouldGiveCompletions ".."

  "[].maxBy.targe▶" shouldGiveCompletions "target"

  // "../m" shouldGiveCompletions "../mash"

  // To get this to work, we need to be able to identify the name corresponding to the nth argument (by syntactical position)
  // This can be done by ParameterModel.bindTypes
  // "map --sequence=ls permiss▶" shouldGiveCompletions ("permissions")

  """ "${user.fullNam▶} """ shouldGiveCompletions "fullName"
  """ "$user.fullNam▶ """ shouldGiveCompletions "fullName"

  """ dollar▶ """ shouldGiveCompletions """dollar\$ign"""
  """ dollar\$i▶ """ shouldGiveCompletions """dollar\$ign"""
  """ dollar\$▶ """ shouldGiveCompletions """dollar\$ign"""
  """ dollar$▶ """ shouldGiveCompletions """dollar\$ign"""
  """ "dollar"▶ """ shouldGiveCompletions """dollar\$ign"""
  """ "dollar\$i"▶ """ shouldGiveCompletions """dollar\$ign"""
  """ "dollar\$"▶ """ shouldGiveCompletions """dollar\$ign"""
  """ "dollar$"▶ """ shouldGiveCompletions """dollar\$ign"""

  """ quotation▶ """ shouldGiveCompletions """quotation\"mark"""
  """ "quotation▶""" shouldGiveCompletions """quotation\"mark"""

  // Would like to get these cases working too:
  //  """ quotation\"mar▶ """ shouldGiveCompletions """quotation\"mark"""
  //  """ quotation"▶ """ shouldGiveCompletions """quotation\"mark"""
  //  """ "quotation\"▶""" shouldGiveCompletions """quotation\"mark"""
  //  """ quotation\"▶ """ shouldGiveCompletions """quotation\"mark"""
  //  """ "quotation▶ """ shouldGiveCompletions """quotation\"mark"""

  // tilde
  "~/di▶" shouldGiveCompletions "~/dir/"

  // Inheritance: just one completion
  "pwd.info.toStrin▶" shouldGiveCompletions "toString"

  "{}.toStrin▶" shouldGiveCompletions "toString"

  // """ps | where (_.owner == "roo▶""" shouldGiveCompletions "root"

  "Seq completions" should "not be marked as vectorised" in {
    val Some(completion) = "pwd.children.isEmpt▶".fullCompletions.find(_.text == "isEmpty")
    val Some(description) = completion.descriptionOpt
    description should not include ("vectorised")
  }

  private implicit class RichString(s: String) {

    def shouldGiveCompletions(expectedCompletions: String*) {
      val expectedDescription = expectedCompletions.mkString(", ")
      "Completer" should s"offer completions for '$s': $expectedDescription" in {
        completions shouldEqual (expectedCompletions)
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

    def completions: Seq[String] = fullCompletions.map(_.text)

    def fullCompletions: Seq[Completion] = {
      val lineBuffer = LineBufferTestHelper.parseLineBuffer(s)
      val env = Environment.create
      val envInteractions = MockEnvironmentInteractions(Paths.get("/home/mash"))
      val completer = new UberCompleter(fileSystem, envInteractions)
      completer.complete(lineBuffer.text, lineBuffer.cursorPos, env, mish = false).map(_.completions).getOrElse(Seq())
    }

    private val fileSystem: FileSystem = {
      val root = MockFileObject.Directory(ListMap(
        "home" -> MockFileObject.Directory(ListMap(
          "mash" -> MockFileObject.Directory(ListMap(
            "dir" -> MockFileObject.Directory(ListMap()),
            "spaces in name" -> MockFileObject.File(),
            "readme.txt" -> MockFileObject.File(),
            ".mashrc" -> MockFileObject.File(),
            "foo.txt" -> MockFileObject.File(),
            "dollar$ign" -> MockFileObject.File(),
            "quotation\"mark" -> MockFileObject.File()))))))
      new MockFileSystem(root, Paths.get("/home/mash"))
    }

  }
}
