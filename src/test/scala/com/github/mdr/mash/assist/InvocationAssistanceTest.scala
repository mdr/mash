package com.github.mdr.mash.assist

import com.github.mdr.mash.assist.InvocationAssistance.getTypeOfNearestFunction
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.collections.{ ListClass, ReverseFunction }
import com.github.mdr.mash.ns.core.{ AnyClass, NumberClass, StringClass }
import com.github.mdr.mash.ns.os.{ HomeFunction, ListFilesFunction }
import com.github.mdr.mash.repl.LineBufferTestHelper
import org.scalatest.{ FlatSpec, Matchers }

class InvocationAssistanceTest extends FlatSpec with Matchers {

  "ls▶" ==> Type.BuiltinFunction(ListFilesFunction)
  "l▶s" ==> Type.BuiltinFunction(ListFilesFunction)
  "▶ls" ==> Type.BuiltinFunction(ListFilesFunction)
  "ls ▶" ==> Type.BuiltinFunction(ListFilesFunction)
  "ls 'dir'▶" ==> Type.BuiltinFunction(ListFilesFunction)
  "ls --recursive▶" ==> Type.BuiltinFunction(ListFilesFunction)

  "ls home▶" ==> Type.BuiltinFunction(HomeFunction)
  "ls ▶home" ==> Type.BuiltinFunction(HomeFunction)
  "ls▶ home" ==> Type.BuiltinFunction(ListFilesFunction)

  "ls (▶home)" ==> Type.BuiltinFunction(HomeFunction)
  "ls ▶(home)" ==> Type.BuiltinFunction(ListFilesFunction)

  "reverse▶" ==> Type.BuiltinFunction(ReverseFunction)

  "1.toString▶" ==> Type.BoundBuiltinMethod(NumberClass, AnyClass.ToStringMethod)
  "''.startsWith 'x'▶" ==> Type.BoundBuiltinMethod(StringClass, StringClass.StartsWithMethod)

  implicit class RichString(s: String) {

    def ==>(expectedType: Type) {
      s"Asking for invocation assistance for '$s'" should s"result in help for type $expectedType" in {
        val buffer = LineBufferTestHelper.parseLineBuffer(s)
        val bindings = StandardEnvironment.create.bindings
        val actualTypeOpt = getTypeOfNearestFunction(buffer.text, buffer.cursorOffset, bindings, mish = false)
        Some(expectedType) should equal(actualTypeOpt)
      }
    }

  }

}
