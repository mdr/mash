package com.github.mdr.mash.completions

import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ ConcreteSyntax, SourceInfo }
import com.github.mdr.mash.utils.{ Region, StringUtils }

import scala.PartialFunction._

case class MemberCompletionResult(isMemberExpr: Boolean,
                                  completionResultOpt: Option[CompletionResult],
                                  prioritiseMembers: Boolean)

object MemberCompleter {

  private val Dummy = "dummy"

  /**
    * Find completions for string literals that correspond to members of the given target type.
    */
  def completeStringMember(targetType: Type, prefix: String): Seq[Completion] =
    MemberFinder.getMembers(targetType)
      .filter(_.name startsWith prefix)
      .map(_.asCompletion(isQuoted = true))

  def completeIdentifier(text: String, identifier: Token, parser: CompletionParser): MemberCompletionResult = {
    val expr = parser.parse(text)
    val memberLikeOpt = findMemberLike(expr, identifier)
    val completionResultOpt =
      for {
        memberLike ← memberLikeOpt
        target = getTarget(memberLike)
        members ← getMembers(target)
        completions = members.filter(_.name startsWith identifier.text).map(_.asCompletion(isQuoted = false))
        result ← CompletionResult.of(completions, identifier.region)
      } yield result
    val prioritiseMembers = memberLikeOpt.exists {
      case memberExpr: MemberExpr ⇒ shouldPrioritise(memberExpr)
      case _                      ⇒ false
    }
    MemberCompletionResult(memberLikeOpt.isDefined, completionResultOpt, prioritiseMembers)
  }

  def completeImmediatelyAfterDot(text: String, dot: Token, pos: Int, parser: CompletionParser): Option[CompletionResult] =
    for {
      (expr, identifier) ← insertDummyIdentifierAfterDot(text, dot, parser)
      memberLikeOpt ← findMemberLike(expr, identifier)
      target = getTarget(memberLikeOpt)
      members ← getMembers(target)
      completions = members.map(_.asCompletion(isQuoted = false))
      region = Region(dot.region.posAfter, 0)
      result ← CompletionResult.of(completions, region)
    } yield result

  private def insertDummyIdentifierAfterDot(text: String, dot: Token, parser: CompletionParser): Option[(Expr, Token)] = {
    val replacedText = StringUtils.replace(text, dot.region, s"${dot.text}$Dummy ")
    val dummyIdentifierRegion = Region(dot.region.posAfter, Dummy.length)
    val expr = parser.parse(replacedText)
    for {
      sourceInfo ← expr.sourceInfoOpt
      identifierToken ← sourceInfo.node.tokens.find(t ⇒ t.isIdentifier && t.region == dummyIdentifierRegion)
    } yield (expr, identifierToken)
  }

  private def getTarget(expr: Expr) = expr match {
    case memberExpr: MemberExpr           ⇒ memberExpr.target
    case importStatement: ImportStatement ⇒ importStatement.expr
    case _                                ⇒ throw new AssertionError(s"Unexpected expression: $expr")
  }

  private def getMembers(target: Expr): Option[Seq[MemberInfo]] =
    target.constantValueOpt.flatMap(MemberFinder.getValueMembers) orElse target.typeOpt.map(MemberFinder.getMembers(_))

  private def shouldPrioritise(memberExpr: MemberExpr): Boolean =
    !spaceBeforeDot(memberExpr) && !cond(memberExpr.target) {
      case _: Identifier | _: StringLiteral ⇒ true
    }

  private def spaceBeforeDot(memberExpr: MemberExpr): Boolean =
    memberExpr.sourceInfoOpt.exists { sourceInfo ⇒
      cond(sourceInfo.node) {
        case ConcreteSyntax.MemberExpr(before, dot, after) ⇒
          dot.offset > 0 && dot.source.charAt(dot.offset - 1).isWhitespace
      }
    }

  private def findMemberLike(expr: Expr, token: Token): Option[Expr] = expr.find {
    case e@MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.MemberExpr(_, _, `token`))))           ⇒ e
    case e@MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.HeadlessMemberExpr(_, `token`))))      ⇒ e
    case e@ImportStatement(_, _, Some(SourceInfo(_, ConcreteSyntax.ImportStatement(_, _, _, `token`)))) ⇒ e
  }

}