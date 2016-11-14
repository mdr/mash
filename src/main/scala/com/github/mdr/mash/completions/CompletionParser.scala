package com.github.mdr.mash.completions

import com.github.mdr.mash.compiler.{ BareStringify, CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.lexer.{ MashLexer, Token }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.runtime.MashValue

case class CompletionParser(env: Map[String, MashValue], mish: Boolean) {

  def parse(s: String): Expr = {
    val settings = CompilationSettings(inferTypes = true)
    Compiler.compileForgiving(CompilationUnit(s, mish = mish), env, settings)
  }

  def tokenise(s: String): Seq[Token] =
    MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens

  def getBareTokens(s: String): Seq[Token] = {
    val settings = CompilationSettings(bareWords = false)
    val expr = Compiler.compileForgiving(CompilationUnit(s, mish = mish), env, settings)
    BareStringify.getBareTokens(expr, env.keySet).toSeq
  }

}
