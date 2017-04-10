package com.github.mdr.mash.evaluator

class NamedArgsTest extends AbstractEvaluatorTest {

  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --foo=1" ==> "{ foo: 1 }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --foo" ==> "{ foo: true }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -a" ==> "{ a: true }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -abc" ==> "{ a: true, b: true, c: true }"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject" ==> "{}"
  "def makeObject (@namedArgs namedArgs) (@flag otherArg) = { namedArgs, otherArg }; makeObject --otherArg=10" ==>
    "{ namedArgs: {}, otherArg: 10 }"
  "def makeObject otherParam (@namedArgs namedArgs) = [otherParam, namedArgs]; makeObject 1 --foo=2" ==> "[1, { foo: 2 }]"
  "def makeObject otherParam (@namedArgs namedArgs) = [otherParam, namedArgs]; makeObject --foo=2 --otherParam=1" ==>
    "[1, { foo: 2 }]"
  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --namedArgs=42" ==> "{ namedArgs: 42 }"
  "def makeObject varargs... (@namedArgs namedArgs) = { varargs, namedArgs }; makeObject 1 2 3 --foo=42" ==>
    "{ varargs: [1, 2, 3], namedArgs: { foo: 42} }"

  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject 1" shouldThrowAnException

  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject --arg --arg" shouldThrowAnException

  "def makeObject (@namedArgs namedArgs) = namedArgs; makeObject -aa" shouldThrowAnException

  "def makeObject arg (@namedArgs namedArgs) = namedArgs; makeObject 42 --arg=100" shouldThrowAnException

}
