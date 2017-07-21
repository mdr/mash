package com.github.mdr.mash.repl

import com.github.mdr.mash.repl.IncrementalHistorySearchState.HitStatus
import com.github.mdr.mash.utils.Region

object IncrementalHistorySearchState {

  sealed trait HitStatus {

    def matchRegionOpt: Option[Region] = None

  }

  case object BeforeFirstHit extends HitStatus

  /**
    * @param resultIndex index into the stream of results that match the searchString
    * @param matchRegion region in the line buffer that matches the search
    */
  case class Hit(resultIndex: Int, matchRegion: Region) extends HitStatus {

    override def matchRegionOpt: Option[Region] = Some(matchRegion)

  }

  case object AfterLastHit extends HitStatus

}

case class IncrementalHistorySearchState(searchString: String = "",
                                         hitStatus: HitStatus)

