package com.github.mdr.mash.repl.history

/**
 * @param resultIndex -- when there are multiple results that match the searchString, the user can select through them.
 *                       This records which result the user is currently viewing.
 */
case class HistorySearchState(searchString: String = "", resultIndex: Int = 0)