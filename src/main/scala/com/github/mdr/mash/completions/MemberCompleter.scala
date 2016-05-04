package com.github.mdr.mash.completions

import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.ns.core.FunctionClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.SourceInfo
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.BoundMethodClass

object MemberCompleter {

  def complete(s: String, nearbyToken: Token, env: Environment, pos: Int, mish: Boolean): Option[CompletionResult] = {
    val replaced = StringUtils.replace(s, nearbyToken.region, ".dummy")
    val dummyRegion = nearbyToken.region.copy(offset = nearbyToken.region.offset + 1)
    val exprOpt = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
    for {
      expr ← exprOpt
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      identifierToken ← tokens.find(t ⇒ t.isIdentifier && t.region.overlaps(dummyRegion))
      memberExpr ← findMemberExpr(expr, identifierToken)
      receiverType ← memberExpr.expr.typeOpt
      members = getMembers(receiverType)
      completions = members.map(_.asCompletion(isQuoted = false))
      if completions.nonEmpty
    } yield CompletionResult(completions, Region(nearbyToken.region.posAfter, 0))
  }

  def complete(targetType: Type, prefix: String) =
    getMembers(targetType).filter(_.name startsWith prefix).map(_.asCompletion(isQuoted = true))

  def completeMember(s: String, nearbyToken: Token, env: Environment, mish: Boolean): (Boolean, Option[CompletionResult]) = {
    val exprOpt = Compiler.compile(s, env, forgiving = true, inferTypes = true, mish = mish)
    val memberExprOpt = for {
      expr ← exprOpt
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      memberExpr ← findMemberExpr(expr, nearbyToken)
    } yield memberExpr
    val completionResultOpt =
      for {
        memberExpr ← memberExprOpt
        receiverType ← memberExpr.expr.typeOpt
        members = getMembers(receiverType)
        completions = members.filter(_.name startsWith nearbyToken.text).map(_.asCompletion(isQuoted = false))
        if completions.nonEmpty
      } yield CompletionResult(completions, nearbyToken.region)
    (memberExprOpt.isDefined, completionResultOpt)
  }

  private def findMemberExpr(e: Expr, token: Token): Option[MemberExpr] = e.find {
    case mexpr @ MemberExpr(_, _, _, Some(SourceInfo(ConcreteSyntax.MemberExpr(_, _, `token`)))) ⇒ mexpr
  }

  private def getMembers(klass: MashClass): Seq[MemberInfo] = {
    val fieldMembers = klass.fields.map(f ⇒ MemberInfo(f.name, classOpt = Some(klass), descriptionOpt = Some(f.summary), isField = true))
    val methodMembers = klass.methods.map(m ⇒ MemberInfo(m.name, classOpt = Some(klass), descriptionOpt = Some(m.summary), isField = false))
    val parentClassMembers = klass.parentOpt.toSeq.flatMap(parentClass ⇒ getMembers(parentClass))
    val members = fieldMembers ++ methodMembers ++ parentClassMembers
    distinct(members)
  }

  private def getMembers(t: Type, canVectorise: Boolean = true): Seq[MemberInfo] = t match {
    case Type.Instance(klass)             ⇒ getMembers(klass)
    case Type.Object(fields)              ⇒ distinct(fields.keys.toSeq.map(f ⇒ MemberInfo(f, isField = true)) ++ getMembers(ObjectClass))
    case Type.Tagged(baseClass, tagClass) ⇒ distinct(getMembers(tagClass) ++ getMembers(baseClass))
    case Type.Group(keyType, elementType) ⇒ getMembers(GroupClass)
    case Type.DefinedFunction(_)          ⇒ getMembers(FunctionClass)
    case Type.BoundMethod(_, _)           ⇒ getMembers(BoundMethodClass)
    case Type.Seq(elementType) ⇒
      val seqMembers = getMembers(SeqClass)
      if (canVectorise) {
        val elementMembers = getMembers(elementType, canVectorise = false).map(_.copy(isVectorised = true))
        distinct(seqMembers ++ elementMembers)
      } else
        seqMembers
    case _ ⇒ Seq()
  }

  private def distinct(members: Seq[MemberInfo]) = Utils.distinctBy[MemberInfo, String](members, _.name)

}

case class MemberInfo(
    name: String,
    isField: Boolean,
    descriptionOpt: Option[String] = None,
    classOpt: Option[MashClass] = None,
    isVectorised: Boolean = false) {

  def isMethod = !isField

  private def description: String = {
    val vecPrefix = if (isVectorised) "vectorised, " else ""
    val fieldOrMethod = if (isField) "field" else "method"
    val classOrigin = classOpt.flatMap(_.nameOpt).map(" in " + _).getOrElse("")
    val desc = descriptionOpt.getOrElse("")
    s"$desc ($vecPrefix$fieldOrMethod$classOrigin)"
  }

  private def completionType: CompletionType = if (isField) CompletionType.Field else CompletionType.Method

  def asCompletion(isQuoted: Boolean) =
    Completion(name, isQuoted = isQuoted, typeOpt = Some(completionType), descriptionOpt = Some(description))

}