package com.github.mdr.mash.compiler

import com.github.mdr.mash.inference.TypeInferencer
import com.github.mdr.mash.inference.ValueTypeDetector
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.parser.Provenance
import com.github.mdr.mash.parser.ConcreteSyntax

case class CompilationSettings(forgiving: Boolean = true, inferTypes: Boolean = false, bareWords: Boolean = true)

case class CompilationUnit(text: String, name: String = "N/A", interactive: Boolean = false, mish: Boolean = false) {

  def provenance = Provenance(name = name, source = text)

}

object Compiler {

  /**
   * Compile the given program into an expression.
   *
   * @param forgiving - if true, recover from syntax errors. If false, syntax errors will raise an exception.
   * @param inferTypes - if true, the resultant expression will be annotated with types, if it was possible to infer them.
   * @param bareWords - if true, unbound identifiers are promoted to string literals
   *
   * @return an expr, if found or else None
   */
  @throws[MashParserException]
  def compile(
    compilationUnit: CompilationUnit,
    bindings: Map[String, MashValue],
    settings: CompilationSettings = CompilationSettings()): Option[Expr] = {
    val concreteExprOpt = MashParser.parse(compilationUnit.text, forgiving = settings.forgiving, mish = compilationUnit.mish)
    concreteExprOpt.map(expr ⇒
      compile(expr, compilationUnit, settings, bindings))
  }

  private def compile(concreteExpr: ConcreteSyntax.Expr, compilationUnit: CompilationUnit, settings: CompilationSettings, bindings: Map[String, MashValue]): Expr = {
    val abstractExpr = new Abstractifier(compilationUnit.provenance).abstractify(concreteExpr)
    val withoutHeadlessMembers = AddHolesToHeadlessMembers.addHoles(abstractExpr)
    val withoutHoles = DesugarHoles.desugarHoles(withoutHeadlessMembers)
    val withoutParens = ParenRemover.removeParens(withoutHoles)
    val withoutPipes = DesugarPipes.desugarPipes(withoutHoles)
    val bareStringified =
      if (settings.bareWords)
        BareStringify.bareStringify(withoutPipes, bindings.keySet)
      else
        withoutPipes
    val finalExpr = bareStringified

    if (settings.inferTypes) {
      val typeInferencer = new TypeInferencer
      val typeBindings = new ValueTypeDetector().buildBindings(bindings)
      typeInferencer.inferType(finalExpr, typeBindings)
    }
    finalExpr
  }

}