package com.github.mdr.mash.printer.model

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.StyledString
import com.github.mdr.mash.utils.Region

sealed trait LinkPath

object LinkPath {

  case class Relative(fragment: String) extends LinkPath

  case class Absolute(path: String) extends LinkPath

}

case class Link(line: Int,
                region: Region,
                target: MashValue,
                linkPath: LinkPath)

case class HelpModel(rawValue: MashValue,
                     lines: Seq[StyledString],
                     links: Seq[Link]) extends DisplayModel {

  def numberOfRows = lines.size

  def numberOfLinks = links.size

}
