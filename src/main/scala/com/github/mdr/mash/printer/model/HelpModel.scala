package com.github.mdr.mash.printer.model

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.StyledString
import com.github.mdr.mash.utils.Region

case class Link(line: Int, region: Region, target: MashValue)

case class HelpModel(rawValue: MashValue,
                     lines: Seq[StyledString],
                     links: Seq[Link]) extends DisplayModel {

  def numberOfLinks = links.size

}
