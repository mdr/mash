package com.github.mdr.mash.evaluator

class GroupByTest extends AbstractEvaluatorTest {

  "[{ path: 'file.txt', type: 'file' }, { path: 'script.mash', type: 'file' }, { path: 'src', type: 'dir' }] | groupBy 'type'" ==>
    "{ file: [{ path: 'file.txt', type: 'file' }, { path: 'script.mash', type: 'file' }], dir: [{ path: 'src', type: 'dir' }] }"
  "[{ path: 'file.txt', type: 'file' }, { path: '000', type: null }] | groupBy 'type'" ==>
    "{ file: [{ path: 'file.txt', type: 'file' }] }"

  "['a', 'bb', 'c', 'ddd'] | groupBy length" ==> "{ (1): ['a', 'c'], (2): ['bb'], (3): ['ddd'] }"

  // --groups
  "[{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 2, b: 3 }, { a: 1, b: 4 }] | groupBy (.a) --groups | map (.unbless)" ==>
    "[{ key: 1, values: [{ a: 1, b: 1 }, { a: 1, b: 2 }, { a: 1, b: 4 }] }, { key: 2, values: [{ a: 2, b: 3 }] }]"

  "'foo' | groupBy (x => x) --groups | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "
  "'foo'.groupBy (x => x) --groups | select 'key' 'count' | sortBy 'key'" ==>
    "[ { key: 'f', count: 1 }, { key: 'o', count: 2 } ] "

  // --includeNull
  "[null] | groupBy --includeNull (x => x) --groups | select 'key' 'count'" ==>
    "[ { key: null, count: 1 } ]"
  "[null] | groupBy --includeNull='nope' (x => x) --groups | select 'key'" ==>
    "[ { key: 'nope' } ]"

  // --all
  "[1, 2, 1] | groupBy --all (x => x) --groups | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'All', count: 3 } ]"
  "[1, 2, 1] | groupBy --all='totalCount' (x => x) --groups | select 'key' 'count' | sortBy 'count'" ==>
    "[ { key: 2, count: 1 }, { key: 1, count: 2 }, { key: 'totalCount', count: 3 } ]"

  // Group.count
  "['apple', 'bike', 'book'] | groupBy first --groups | sortBy (.key) | map (.count)" ==> "[1, 2]"

  // --select
  "[{ path: 'file.txt', type: 'file' }, { path: 'src', type: 'dir' }] | groupBy 'type' --select='path' --groups | select 'key' 'values' | sortBy 'key'" ==>
    "[{ key: 'dir', values: ['src'] }, { key: 'file', values: ['file.txt'] }]"

}
