package com.github.mdr.mash.view.model

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

import scala.collection.immutable.ListMap

case class SingleObjectTableModel(classNameOpt: Option[String],
                                  fields: ListMap[String, String],
                                  fieldColumnWidth: Int,
                                  valueColumnWidth: Int,
                                  rawValue: MashObject,
                                  rawValues: ListMap[MashValue, MashValue]) extends DisplayModel {
  require(fields.nonEmpty)

  def numberOfRows: Int = fields.size

}