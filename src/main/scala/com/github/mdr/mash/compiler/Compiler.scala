package com.github.mdr.mash.compiler

import com.github.mdr.mash.inference.TypeInferencer
import com.github.mdr.mash.inference.ValueTypeDetector
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser._
import com.github.mdr.mash.runtime.MashValue

case class CompilationSettings(inferTypes: Boolean = false, bareWords: Boolean = true)

case class CompilationUnit(text: String, name: String = "N/A", interactive: Boolean = false, mish: Boolean = false) {

  def provenance = Provenance(name = name, source = text)

}

object Compiler {

  /**
   * Compile the given program into an expression.
   */
  def compileForgiving(compilationUnit: CompilationUnit,
                       bindings: Map[String, MashValue],
                       settings: CompilationSettings = CompilationSettings()): Expr = {
    val concreteExpr = MashParser.parseForgiving(compilationUnit.text, mish = compilationUnit.mish)
    compile(concreteExpr, compilationUnit, settings, bindings)
  }

  def compile(compilationUnit: CompilationUnit,
              bindings: Map[String, MashValue],
              settings: CompilationSettings = CompilationSettings()): Either[ParseError, Expr] =
    MashParser.parse(compilationUnit.text, mish = compilationUnit.mish).right.map(concreteExpr ⇒
      compile(concreteExpr, compilationUnit, settings, bindings))

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