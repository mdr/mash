package com.github.mdr.mash.completions

import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.compiler.CompilationSettings

case class CompletionParser(env: Map[String, MashValue], mish: Boolean) {

  def parse(s: String): Option[Expr] =
    Compiler.compile(CompilationUnit(s, mish = mish), env, CompilationSettings(forgiving = true, inferTypes = true))

  def tokenise(s: String): Seq[Token] =
    MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)

  def getBareTokens(s: String): Seq[Token] =
    Compiler.compile(CompilationUnit(s, mish = mish), env, CompilationSettings(forgiving = true, bareWords = false)).map(expr ⇒
      BareStringify.getBareTokens(expr, env.keySet).toSeq).getOrElse(Seq())

}
