package com.github.mdr.mash.incrementalSearch

/**
 * @param resultIndex -- when there are multiple results that match the searchString, the user can select through them.
 *                       This records which result the user is currently viewing.
 */
case class IncrementalSearchState(searchString: String = "", resultIndex: Int = 0)