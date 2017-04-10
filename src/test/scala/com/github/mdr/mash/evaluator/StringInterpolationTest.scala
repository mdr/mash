package com.github.mdr.mash.evaluator

class StringInterpolationTest extends AbstractEvaluatorTest {

  """(name => "Hello $name!") "Matt" """ ==> "'Hello Matt!'"
  """(name => "Hello ${name}!") "Matt" """ ==> "'Hello Matt!'"
  """(name => "Hello $name.reverse!") "Matt" """ ==> "'Hello ttaM!'"
  """(name => "Hello ${name.reverse}!") "Matt" """ ==> "'Hello ttaM!'"
  """(name => "Hello ${name | reverse}!") "Matt" """ ==> "'Hello ttaM!'"
  """ 42 | "$_" """ ==> "'42'"
  """ 'foo' | "$_.reverse" """ ==> "'oof'"
  """ "`"${42}`"" """ ==> """ '"42"' """
  """ "'${42}'" """ ==> """ "'42'" """
  """ "${4}'bar'${2}" """ ==> """ "4'bar'2" """

}
