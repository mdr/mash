package com.github.mdr.mash.completions

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core.{ BoundMethodClass, FunctionClass, ObjectClass }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ ConcreteSyntax, SourceInfo }
import com.github.mdr.mash.utils.{ Region, StringUtils, Utils }

import scala.PartialFunction.cond

case class MemberCompletionResult(isMemberExpr: Boolean,
                                  completionResultOpt: Option[CompletionResult],
                                  prioritiseMembers: Boolean)

object MemberCompleter {

  private val Dummy = "dummy"

  def completeAfterDot(text: String, dot: Token, pos: Int, parser: CompletionParser): Option[CompletionResult] = {
    val replacedText = StringUtils.replace(text, dot.region, s"${dot.text}$Dummy ")
    val dummyIdentifierRegion = Region(dot.region.posAfter, Dummy.length)
    val expr = parser.parse(replacedText)
    for {
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

  private def findMemberExpr(text: String, identifier: Token, parser: CompletionParser): Option[MemberExpr] = {
    val expr = parser.parse(text)
    for {
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      memberExpr ← findMemberExpr(expr, identifier)
    } yield memberExpr
  }

  private def findMemberExpr(expr: Expr, token: Token): Option[MemberExpr] = expr.find {
    case mexpr@MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.MemberExpr(_, _, `token`))))      ⇒ mexpr
    case mexpr@MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.HeadlessMemberExpr(_, `token`)))) ⇒ mexpr
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

  private def getMembers(userClass: UserClass): Seq[MemberInfo] = {
    val fieldMembers = userClass.params.params.flatMap(_.nameOpt).map(name ⇒ MemberInfo(name, isField = true))
    val methodMembers = userClass.methods.map { case (name, method) ⇒ MemberInfo(name, isField = false) }
    val parentClassMembers = getMembers(ObjectClass)
    val members = fieldMembers ++ methodMembers ++ parentClassMembers
    distinct(members)
  }

  private def getMembers(t: Type, canVectorise: Boolean = true): Seq[MemberInfo] = t match {
    case Type.Instance(klass)              ⇒ getMembers(klass)
    case Type.UserClassInstance(userClass) ⇒ getMembers(userClass)
    case Type.Tagged(baseClass, tagClass)  ⇒ distinct(getMembers(tagClass) ++ getMembers(baseClass))
    case Type.Generic(klass, _*)           ⇒ getMembers(klass)
    case Type.BuiltinFunction(_)           ⇒ getMembers(FunctionClass)
    case Type.BoundBuiltinMethod(_, _)     ⇒ getMembers(BoundMethodClass)
    case Type.Object(fields)               ⇒
      val fieldMembers = fields.keys.toSeq.map(f ⇒ MemberInfo(f, isField = true))
      distinct(fieldMembers ++ getMembers(ObjectClass))
    case Type.Seq(elementType)             ⇒
      val seqMembers = getMembers(ListClass)
      if (canVectorise) {
        val elementMembers = getMembers(elementType, canVectorise = false).map(_.copy(isVectorised = true))
        distinct(seqMembers ++ elementMembers)
      } else
        seqMembers
    case _                                 ⇒ Seq()
  }

  private def distinct(members: Seq[MemberInfo]) = Utils.distinctBy[MemberInfo, String](members, _.name)

}

case class MemberInfo(name: String,
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