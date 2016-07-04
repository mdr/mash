package com.github.mdr.mash.compiler

import com.github.mdr.mash.inference.TypeInferencer
import com.github.mdr.mash.inference.ValueTypeDetector
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.parser.Provenance

case class CompilationUnit(text: String, name: String = "N/A", interactive: Boolean = false) {
  
  def provenance = Provenance(name = name, source = text)
  
}

object Compiler {

  /**
   * Compile the given String into an expression.
   *
   * @param forgiving - if true, recover from syntax errors. If false, syntax errors will raise an exception.
   * @param inferTypes - if true, the resultant expression will be annotated with types, if it was possible to infer them.
   * @param mish - if true, parse outermost expression as mish by default
   * @param bareWords - if true, unbound identifiers are promoted to string literals
   * 
   * @return an expr, if found or else None
   */
  @throws[MashParserException]
  def compile(
    compilationUnit: CompilationUnit,
    bindings: Map[String, MashValue],
    forgiving: Boolean = true,
    inferTypes: Boolean = false,
    mish: Boolean = false,
    bareWords: Boolean = true): Option[Expr] =
    MashParser.parse(compilationUnit.text, forgiving = forgiving, mish = mish).map { concreteExpr ⇒
      val provenance = Provenance(compilationUnit.name, compilationUnit.text)
      val abstractExpr = new Abstractifier(provenance).abstractify(concreteExpr)
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