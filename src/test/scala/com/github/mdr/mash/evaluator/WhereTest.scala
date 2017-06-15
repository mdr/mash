package com.github.mdr.mash.evaluator

class WhereTest extends AbstractEvaluatorTest {

  // where
  "[1, 2, 3, 2, 1] | where (_ > 2)" ==> "[3]"
  "[1, 2, 3, 2, 1]. where (_ > 2)" ==> "[3]"

  "'foobar' | where (_ > 'm')" ==> "'oor'"
  "'foobar' | where (_ => false)" ==> "''"
  "'foobar'.toPath | where (_ > 'm') | .tag" ==> "os.Path"
  "'foobar'.where (_ > 'm')" ==> "'oor'"

  "{ apple: 1, bob: 2, aardvark: 3 }.where (.startsWith 'a')" ==> "{ apple: 1, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 }.where (f v => f.startsWith 'b' or v == 3)" ==> "{ bob: 2, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | where (.startsWith 'a')" ==> "{ apple: 1, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | where (f v => f.startsWith 'b' or v == 3)" ==> "{ bob: 2, aardvark: 3 }"
  "{ apple: 1, bob: 2, aardvark: 3 }.where (f v i => i == 1)" ==> "{ bob: 2 }"

  // whereNot
  "[1, 2, 3, 2, 1] | whereNot (_ > 2)" ==> "[1, 2, 2, 1]"
  "[1, 2, 3, 2, 1].whereNot (_ > 2)" ==> "[1, 2, 2, 1]"

  "'foobar' | whereNot (_ > 'm')" ==> "'fba'"
  "'foobar'.whereNot (_ > 'm')" ==> "'fba'"

  "{ apple: 1, bob: 2, aardvark: 3 }.whereNot (.startsWith 'a')" ==> "{ bob: 2 }"
  "{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v => f.startsWith 'b' or v == 3)" ==> "{ apple: 1 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | whereNot (.startsWith 'a')" ==> "{ bob: 2 }"
  "{ apple: 1, bob: 2, aardvark: 3 } | whereNot (f v => f.startsWith 'b' or v == 3)" ==> "{ apple: 1 }"
  "{ apple: 1, bob: 2, aardvark: 3 }.whereNot (f v i => i == 1)" ==> "{ apple: 1, aardvark: 3 }"

}
