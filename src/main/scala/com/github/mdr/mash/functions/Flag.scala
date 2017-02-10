package com.github.mdr.mash.functions

case class Flag(descriptionOpt: Option[String],
                 shortNameOpt: Option[String] = None,
                 longNameOpt: Option[String] = None)
