package com.github.mdr.mash.compiler

import com.github.mdr.mash.inference.TypeInferencer
import com.github.mdr.mash.inference.ValueTypeDetector
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.runtime.MashValue

object Compiler {

  /**
   * Compile the given String into an expression.
   *
   * @param forgiving - if true, recover from syntax errors. If false, syntax errors will raise an exception.
   * @param inferTypes - if true, the resultant expression will be annotated with types, if it was possible to infer them.
   *
   * @return an expr, if found or else None
   */
  @throws[MashParserException]
  def compile(
    s: String,
    bindings: Map[String, MashValue],
    forgiving: Boolean = true,
    inferTypes: Boolean = false,
    mish: Boolean = false,
    bareWords: Boolean = true): Option[Expr] =
    MashParser.parse(s, forgiving = forgiving, mish = mish).map { concreteExpr ⇒
      val abstractExpr = Abstractifier.abstractify(concreteExpr)
      val withoutHeadlessMembers = AddHolesToHeadlessMembers.addHoles(abstractExpr)
      val withoutHoles = DesugarHoles.desugarHoles(withoutHeadlessMembers)
      val withoutParens = ParenRemover.removeParens(withoutHoles)
      val withoutPipes = DesugarPipes.desugarPipes(withoutHoles)
      val bareStringified =
        if (bareWords)
          BareStringify.bareStringify(withoutPipes, bindings.keySet)
        else
          withoutPipes
      val finalExpr = bareStringified

      if (inferTypes) {
        val typeInferencer = new TypeInferencer
        val typeBindings = new ValueTypeDetector().buildBindings(bindings)
        typeInferencer.inferType(finalExpr, typeBindings)
      }

      finalExpr
    }

}