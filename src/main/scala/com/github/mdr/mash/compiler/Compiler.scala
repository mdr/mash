package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.inference.TypeInferencer
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.parser.MashParserException

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
    environment: Environment,
    forgiving: Boolean = true,
    inferTypes: Boolean = false,
    mish: Boolean = false,
    bareWords: Boolean = true): Option[Expr] =
    MashParser.parse(s, forgiving = forgiving, mish = mish).map { concreteExpr ⇒
      val abstractExpr = Abstractifier.abstractify(concreteExpr)
      val withoutHoles = DesugarHoles.desugarHoles(abstractExpr)
      val withoutParens = ParenRemover.removeParens(withoutHoles)
      val withoutPipes = DesugarPipes.desugarPipes(withoutHoles)
      val bareStringified =
        if (bareWords)
          BareStringify.bareStringify(withoutPipes, environment.bindings.keySet ++ environment.globalVariables.keySet)
        else
          withoutPipes
      val finalExpr = bareStringified

      if (inferTypes) {
        val typeInferencer = new TypeInferencer
        val typeBindings = TypeInferencer.buildBindings(environment)
        typeInferencer.inferType(finalExpr, typeBindings)
      }

      finalExpr
    }

}