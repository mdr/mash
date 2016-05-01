package com.github.mdr.mash.functions

case class Flag(
  summary: String,
  shortNameOpt: Option[String] = None,
  longNameOpt: Option[String] = None)
