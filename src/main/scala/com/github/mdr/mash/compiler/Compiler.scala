package com.github.mdr.mash.compiler

import com.github.mdr.mash.evaluator.{ EvaluationContext, ScopeStack }
import com.github.mdr.mash.inference.{ SimpleEvaluator, Type, TypeInferencer, ValueTypeDetector }
import com.github.mdr.mash.parser.AbstractSyntax.{ Expr, Program }
import com.github.mdr.mash.parser._
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

case class CompilationSettings(inferTypes: Boolean = false, bareWords: Boolean = false)

case class CompilationUnit(text: String, name: String = "N/A", interactive: Boolean = false, mish: Boolean = false) {

  def provenance = Provenance(name = name, source = text)

}

object Compiler {

  /**
    * Compile the given program.
    */
  def compileForgiving(compilationUnit: CompilationUnit,
                       bindings: Map[String, MashValue],
                       settings: CompilationSettings = CompilationSettings()): Program = {
    val concreteExpr = MashParser.parseForgiving(compilationUnit.text, mish = compilationUnit.mish)
    compile(concreteExpr, compilationUnit, settings, bindings)
  }

  def compile(compilationUnit: CompilationUnit,
              bindings: Map[String, MashValue],
              settings: CompilationSettings = CompilationSettings()): Either[ParseError, Program] =
    MashParser.parse(compilationUnit.text, mish = compilationUnit.mish).right.map(concreteExpr ⇒
      compile(concreteExpr, compilationUnit, settings, bindings))

  private def compile(concreteProgram: ConcreteSyntax.Program,
                      compilationUnit: CompilationUnit,
                      settings: CompilationSettings,
                      bindings: Map[String, MashValue]): Program = {
    val abstractProgram = new Abstractifier(compilationUnit.provenance).abstractify(concreteProgram)
    val withoutHeadlessMembers = AddHolesToHeadlessMembers.addHoles(abstractProgram)
    val withoutHoles = DesugarHoles.desugarHoles(withoutHeadlessMembers)
    val withoutPipes = DesugarPipes.desugarPipes(withoutHoles)
    val bareStringified =
      if (settings.bareWords)
        BareStringify.bareStringify(withoutPipes, bindings.keySet)
      else
        withoutPipes
    val finalProgram = bareStringified

    SimpleEvaluator.evaluate(finalProgram.body)(EvaluationContext(ScopeStack(MashObject.of(bindings))))

    if (settings.inferTypes)
      inferTypes(bindings, finalProgram.body)

    finalProgram
  }

  private def inferTypes(bindings: Map[String, MashValue], finalExpr: Expr): Option[Type] = {
    val typeBindings = new ValueTypeDetector().buildBindings(bindings)
    new TypeInferencer().inferType(finalExpr, typeBindings)
  }

}