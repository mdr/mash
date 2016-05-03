package com.github.mdr.mash.repl

sealed trait ReplMode

object ReplMode {
  case object Normal extends ReplMode
  case object BrowseCompletions extends ReplMode
  case object IncrementalCompletions extends ReplMode
  case object IncrementalSearch extends ReplMode
}