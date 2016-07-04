package com.github.mdr.mash.parser

import com.github.mdr.mash.utils.PointedRegion

case class MashParserException(message: String, location: PointedRegion) extends RuntimeException(message) {

  def parseError = ParseError(message, location)
  
}