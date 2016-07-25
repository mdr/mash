package com.github.mdr.mash.parser

sealed trait RedirectOperator
object RedirectOperator {
  
  case object StandardInput extends RedirectOperator
  case object StandardOutput extends RedirectOperator
  
}