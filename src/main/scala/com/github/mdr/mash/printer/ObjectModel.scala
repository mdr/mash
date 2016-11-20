package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

import scala.collection.immutable.ListMap

case class ObjectModel(fields: ListMap[String, String],
                       fieldColumnWidth: Int,
                       valueColumnWidth: Int,
                       rawValue: MashObject,
                       rawValues: ListMap[String, MashValue]) extends PrintModel
