package com.github.mdr.mash.compiler

import com.github.mdr.mash.evaluator.{ EvaluationContext, ScopeStack }
import com.github.mdr.mash.inference.{ SimpleEvaluator, Type, TypeInferencer, ValueTypeDetector }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser._
import com.github.mdr.mash.runtime.MashValue

import scala.collection.mutable

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
    val withoutPipes = DesugarPipes.desugarPipes(withoutHoles)
    val bareStringified =
      if (settings.bareWords)
        BareStringify.bareStringify(withoutPipes, bindings.keySet)
      else
        withoutPipes
    val finalExpr = bareStringified

    if (settings.inferTypes) {
      SimpleEvaluator.evaluate(finalExpr)(EvaluationContext(ScopeStack(mutable.Map(bindings.toSeq: _*))))
      inferTypes(bindings, finalExpr)
    }

    finalExpr
  }

  private def inferTypes(bindings: Map[String, MashValue], finalExpr: Expr): Option[Type] = {
    val typeBindings = new ValueTypeDetector().buildBindings(bindings)
    new TypeInferencer().inferType(finalExpr, typeBindings)
  }

}