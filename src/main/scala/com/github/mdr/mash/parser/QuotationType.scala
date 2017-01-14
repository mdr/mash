package com.github.mdr.mash.parser

sealed trait QuotationType

object QuotationType {

  case object Double extends QuotationType

  case object Single extends QuotationType

}