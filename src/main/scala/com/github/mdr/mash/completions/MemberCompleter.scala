package com.github.mdr.mash.completions

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core.BoundMethodClass
import com.github.mdr.mash.ns.core.FunctionClass
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.SourceInfo
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils
import scala.PartialFunction.cond
import com.github.mdr.mash.runtime.MashString

case class MemberCompletionResult(
  prioritiseMembers: Boolean,
  completionResultOpt: Option[CompletionResult],
  spaceBeforeDot: Boolean)

object MemberCompleter {

  private val Dummy = "dummy"

  def completeAfterDot(text: String, dot: Token, pos: Int, parser: CompletionParser): Option[CompletionResult] = {
    val replacedText = StringUtils.replace(text, dot.region, s"${dot.text}$Dummy ")
    val dummyIdentifierRegion = Region(dot.region.posAfter, Dummy.length)
    for {
      expr ← parser.parse(replacedText)
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      identifierToken ← tokens.find(t ⇒ t.isIdentifier && t.region == dummyIdentifierRegion)
      memberExpr ← findMemberExpr(expr, identifierToken)
      targetType ← memberExpr.target.typeOpt
      members = getMembers(targetType)
      completions = members.map(_.asCompletion(isQuoted = false))
      region = Region(dot.region.posAfter, 0)
      result ← CompletionResult.of(completions, region)
    } yield result
  }

  def completeString(targetType: Type, prefix: String): Seq[Completion] =
    getMembers(targetType).filter(_.name startsWith prefix).map(_.asCompletion(isQuoted = true))

  def completeIdentifier(text: String, identifier: Token, parser: CompletionParser): MemberCompletionResult = {
    val memberExprOpt = findMemberExpr(text, identifier, parser)
    val completionResultOpt =
      for {
        memberExpr ← memberExprOpt
        targetType ← memberExpr.target.typeOpt
        members = getMembers(targetType)
        completions = members.filter(_.name startsWith identifier.text).map(_.asCompletion(isQuoted = false))
        result ← CompletionResult.of(completions, identifier.region)
      } yield result
    val prioritiseMembers = memberExprOpt.exists(shouldPrioritise)
    MemberCompletionResult(memberExprOpt.isDefined, completionResultOpt, prioritiseMembers)
  }

  private def shouldPrioritise(memberExpr: MemberExpr): Boolean =
    !spaceBeforeDot(memberExpr) && !cond(memberExpr.target) {
      case _: Identifier | _: StringLiteral ⇒ true
    }

  private def spaceBeforeDot(memberExpr: MemberExpr): Boolean =
    memberExpr.sourceInfoOpt.exists { sourceInfo ⇒
      cond(sourceInfo.expr) {
        case ConcreteSyntax.MemberExpr(before, dot, after) ⇒
          dot.offset > 0 && dot.source.charAt(dot.offset - 1).isWhitespace
      }
    }

  private def findMemberExpr(text: String, identifier: Token, parser: CompletionParser): Option[MemberExpr] =
    for {
      expr ← parser.parse(text)
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      memberExpr ← findMemberExpr(expr, identifier)
    } yield memberExpr

  private def findMemberExpr(expr: Expr, token: Token): Option[MemberExpr] = expr.find {
    case mexpr @ MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.MemberExpr(_, _, `token`))))      ⇒ mexpr
    case mexpr @ MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.HeadlessMemberExpr(_, `token`)))) ⇒ mexpr
  }

  private def getMembers(klass: MashClass): Seq[MemberInfo] = {
    val fieldMembers = klass.fields.map(f ⇒
      MemberInfo(f.name, classOpt = Some(klass), descriptionOpt = Some(f.summary), isField = true))
    val methodMembers = klass.methods.map(m ⇒
      MemberInfo(m.name, classOpt = Some(klass), descriptionOpt = Some(m.summary), isField = false))
    val parentClassMembers = klass.parentOpt.toSeq.flatMap(parentClass ⇒ getMembers(parentClass))
    val members = fieldMembers ++ methodMembers ++ parentClassMembers
    distinct(members)
  }

  private def getMembers(t: Type, canVectorise: Boolean = true): Seq[MemberInfo] = t match {
    case Type.Instance(klass)             ⇒ getMembers(klass)
    case Type.Tagged(baseClass, tagClass) ⇒ distinct(getMembers(tagClass) ++ getMembers(baseClass))
    case Type.Group(keyType, elementType) ⇒ getMembers(GroupClass)
    case Type.DefinedFunction(_)          ⇒ getMembers(FunctionClass)
    case Type.BoundMethod(_, _)           ⇒ getMembers(BoundMethodClass)
    case Type.Object(fields) ⇒
      val fieldMembers = fields.keys.toSeq.map(f ⇒ MemberInfo(f, isField = true))
      distinct(fieldMembers ++ getMembers(ObjectClass))
    case Type.Seq(elementType) ⇒
      val seqMembers = getMembers(ListClass)
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