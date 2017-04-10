package com.github.mdr.mash.evaluator

class GroupByTest extends AbstractEvaluatorTest {

  "[1, 2, 3, 1] | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 1, count: 2 }, { key: 2, count: 1 }, { key: 3, count: 1 } ] "
  "'foo' | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "
  "'foo'.groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "

  "[null] | groupBy --includeNull (x => x) | select 'key' 'count'" ==>
    "[ { key: null, count: 1 } ]"
  "[null] | groupBy --includeNull='nope' (x => x) | select 'key'" ==>
    "[ { key: 'nope' } ]"

  "[1, 2, 1] | groupBy --total (x => x) | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'Total', count: 3 } ]"
  "[1, 2, 1] | groupBy --total='totalCount' (x => x) | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'totalCount', count: 3 } ]"

  // Group.count
  "['apple', 'bike', 'book'] | groupBy first | sortBy (.key) | map (.count)" ==> "[1, 2]"

}
