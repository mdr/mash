package com.github.mdr.mash.printer.model

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

import scala.collection.immutable.ListMap

case class SingleObjectTableModel(classNameOpt: Option[String],
                                  fields: ListMap[String, String],
                                  fieldColumnWidth: Int,
                                  valueColumnWidth: Int,
                                  rawValue: MashObject,
                                  rawValues: ListMap[MashValue, MashValue]) extends PrintModel {
  require(fields.nonEmpty)

  def numberOfRows: Int = fields.size

}