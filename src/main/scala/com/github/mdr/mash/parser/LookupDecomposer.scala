package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.parser.MashParser.parseForgiving

import scala.PartialFunction.condOpt

object LookupDecomposer {

  case class NumericLookup(prefix: String, index: Int)

  def decomposeNumericLookup(expr: String): Option[NumericLookup] =
    condOpt(parseForgiving(expr).body) {
      case LookupExpr(prefixExpr, _, Literal(token), _) if token.text matches "0|([1-9][0-9]*)" ⇒
        NumericLookup(prefixExpr.region.of(expr), token.text.toInt)
    }

}
