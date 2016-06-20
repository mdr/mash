package com.github.mdr.mash.completions

import com.github.mdr.mash.compiler.BareStringify
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.parser.AbstractSyntax.Expr

case class CompletionParser(env: Map[String, MashValue], mish: Boolean) {

  def parse(s: String): Option[Expr] =
    Compiler.compile(s, env, forgiving = true, inferTypes = true, mish = mish)

  def tokenise(s: String): Seq[Token] =
    MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)

  def getBareTokens(s: String): Seq[Token] =
    Compiler.compile(s, env, forgiving = true, mish = mish, bareWords = false).map(expr ⇒
      BareStringify.getBareTokens(expr, env.keySet).toSeq).getOrElse(Seq())

}
