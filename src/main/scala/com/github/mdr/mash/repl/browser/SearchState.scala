package com.github.mdr.mash.repl.browser

import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.utils.Region

case class SearchState(query: String, byPoint: Map[Point, CellSearchInfo] = Map(), ignoreCase: Boolean = true) {

  lazy val rows: Seq[Int] = byPoint.map(_._1.row).toSeq.distinct.sorted

  def getCellSearchInfo(point: Point): Option[CellSearchInfo] = byPoint.get(point)

}

case class CellSearchInfo(matches: Seq[Region]) {
  def isMatched(offset: Int): Boolean = matches exists (_ contains offset)
}
