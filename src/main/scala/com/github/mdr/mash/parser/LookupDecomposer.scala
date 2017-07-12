package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.parser.MashParser.parseForgiving
import com.github.mdr.mash.utils.NumberUtils.isInt

import scala.PartialFunction.condOpt

object LookupDecomposer {

  case class NumericLookup(prefix: String, index: Int)

  def decomposeNumericLookup(expr: String): Option[NumericLookup] =
    condOpt(parseForgiving(expr).body) {
      case LookupExpr(prefixExpr, _, Literal(token), _) if isInt(token.text) ⇒
        NumericLookup(prefixExpr.region.of(expr), token.text.toInt)
    }

}
