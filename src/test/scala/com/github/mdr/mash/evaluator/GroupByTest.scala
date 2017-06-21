package com.github.mdr.mash.evaluator

class GroupByTest extends AbstractEvaluatorTest {

  "[{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 2, b: 3 }, { a: 1, b: 4 }] | groupBy (.a) | map (.unbless)" ==>
    "[{ key: 1, values: [{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 1, b: 4 }] }, { key: 2, values: [{ a: 2, b: 3 }] }]"

  "'foo' | groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "
  "'foo'.groupBy (x => x) | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "

  // --includeNull
  "[null] | groupBy --includeNull (x => x) | select 'key' 'count'" ==>
    "[ { key: null, count: 1 } ]"
  "[null] | groupBy --includeNull='nope' (x => x) | select 'key'" ==>
    "[ { key: 'nope' } ]"

  // --total
  "[1, 2, 1] | groupBy --total (x => x) | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'Total', count: 3 } ]"
  "[1, 2, 1] | groupBy --total='totalCount' (x => x) | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'totalCount', count: 3 } ]"

  // Group.count
  "['apple', 'bike', 'book'] | groupBy first | sortBy (.key) | map (.count)" ==> "[1, 2]"

  // --object
  "[{ path: 'file.txt', type: 'file' }, { path: 'script.mash', type: 'file' }, { path: 'src', type: 'dir' }] | groupBy 'type' --object" ==>
    "{ file: [{ path: 'file.txt', type: 'file' }, { path: 'script.mash', type: 'file' }], dir: [{ path: 'src', type: 'dir' }] }"
  "[{ path: 'file.txt', type: 'file' }, { path: '000', type: null }] | groupBy 'type' --object" ==>
    "{ file: [{ path: 'file.txt', type: 'file' }] }"

  "[].groupBy --total --object".shouldThrowAnException

  // --select
  "[{ path: 'file.txt', type: 'file' }, { path: 'src', type: 'dir' }] | groupBy 'type' --select='path' | select 'key' 'values' | sortBy 'key'" ==>
    "[{ key: 'dir', values: ['src'] }, { key: 'file', values: ['file.txt'] }]"

}
