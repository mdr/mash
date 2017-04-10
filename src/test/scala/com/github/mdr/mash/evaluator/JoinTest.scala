package com.github.mdr.mash.evaluator

class JoinTest extends AbstractEvaluatorTest {

  " [1, 2, 3].join(', ') " ==> " '1, 2, 3' "
  " [1, 2, 3].join " ==> " '123' "
  " join ', ' [1, 2, 3] " ==> " '1, 2, 3' "
  " join [1, 2, 3] " ==> " '123' "
  " join --sequence=[1, 2, 3] " ==> " '123' "
  " join ', ' --sequence=[1, 2, 3] " ==> " '1, 2, 3' "
  " join --separator=', ' --sequence=[1, 2, 3] " ==> " '1, 2, 3' "
  " join --separator=', ' [1, 2, 3] " ==> " '1, 2, 3' "
  " join [] " ==> " '' "
  " join 'abc' " ==> " 'abc' "
  " join ':' 'abc' " ==> " 'a:b:c' "
  " 'abc'.join ':'" ==> " 'a:b:c' "

}
