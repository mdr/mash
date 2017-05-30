package com.github.mdr.mash.evaluator

class SortTest extends AbstractEvaluatorTest {

  // sort
  " ['c', 'a', 'b'].sort " ==> "['a', 'b', 'c']"
  "'eaebcd' | sort" ==> "'abcdee'"
  "'eaebcd'.sort" ==> "'abcdee'"
  "[1, null, 2].sort" ==> "[null, 1, 2]"
  "sort [1, 3, 2] --descending" ==> "[3, 2, 1]"
  "sort ['a1.txt', 'a10.txt', 'a2.txt'] --naturalOrder" ==> "['a1.txt', 'a2.txt', 'a10.txt']"
  // Bug where we used lte rather than lt to sort with:
  "sort [15, 12, 9, 3, 2, 90, 75, 7, 18, 9, 2, 1, 1, 14, 3, 3, 2, 21, 53, 2, 61, 24, 31, 1, 13, 14, 21, 4, 28, 17, 2, 5, 1, 17, 3, 3, 10, 100, 246, 176, 2, 10, 2, 4, 1, 1, 2, 1, 261, 1, 27, 10, 3, 6, 390, 44, 1, 2, 4, 1, 13, 4, 6, 1, 2, 8, 9, 3, 33, 9, 3, 131, 10, 2, 15, 35, 2, 157, 71, 32, 4, 12, 6, 7, 3, 8, 43, 8, 35, 1, 1, 11, 4, 2, 1, 9, 1]" shouldNotThrowAnException

  "sort { z: 26, m: 13, b: 2, c: 3, a: 1 } | .fields.name.join" ==> "'abcmz'"
  "{ z: 26, m: 13, b: 2, c: 3, a: 1 }.sort.fields.name.join" ==> "'abcmz'"
  "sort { a1: 1, a10: 10, a2: 10 } --naturalOrder | .fields.name" ==> "['a1', 'a2', 'a10']"
  "sort {}" ==> "{}"

  // sortBy
  " ['aa', 'b', 'ccc'] | sortBy length " ==> " ['b', 'aa', 'ccc'] "
  "'123' | sortBy (-_.toNumber)" ==> "'321'"
  "'123'.sortBy (-_.toNumber)" ==> "'321'"
  "[{ foo: 1 }, { foo: null }, { foo: 2 }].sortBy 'foo'" ==> "[{ foo: null }, { foo: 1 }, { foo: 2 }]"
  "[{ foo: 'a1.txt'}, { foo: 'a10.txt' }, { foo: 'a2.txt' }] | sortBy (.foo) --naturalOrder" ==>
    "[{ foo: 'a1.txt'}, { foo: 'a2.txt' }, { foo: 'a10.txt' }]"

}
