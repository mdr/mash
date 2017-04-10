package com.github.mdr.mash.completions

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ ConcreteSyntax, SourceInfo }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.utils.{ Region, StringUtils, Utils }

import scala.PartialFunction._

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
      tokens = sourceInfo.node.tokens
      identifierToken ← tokens.find(t ⇒ t.isIdentifier && t.region == dummyIdentifierRegion)
      memberExprOrImport ← findMemberExprOrImport(expr, identifierToken)
      target = getTarget(memberExprOrImport)
      members ← getMembers(target)
      completions = members.map(_.asCompletion(isQuoted = false))
      region = Region(dot.region.posAfter, 0)
      result ← CompletionResult.of(completions, region)
    } yield result
  }

  def completeString(targetType: Type, prefix: String): Seq[Completion] =
    getMembers(targetType).filter(_.name startsWith prefix).map(_.asCompletion(isQuoted = true))

  private def getTarget(expr: Expr) = expr match {
    case memberExpr: MemberExpr           ⇒ memberExpr.target
    case importStatement: ImportStatement ⇒ importStatement.expr
    case _                                ⇒ throw new AssertionError(s"Unexpected target: $expr")
  }

  private def getMembers(target: Expr): Option[Seq[MemberInfo]] =
    target.constantValueOpt.flatMap(getValueMembers) orElse target.typeOpt.map(getMembers(_))

  def completeIdentifier(text: String, identifier: Token, parser: CompletionParser): MemberCompletionResult = {
    val exprOpt = findMemberExprOrImport(text, identifier, parser)
    val completionResultOpt =
      for {
        expr ← exprOpt
        target = getTarget(expr)
        targetType ← target.typeOpt
        members ← getMembers(target)
        completions = members.filter(_.name startsWith identifier.text).map(_.asCompletion(isQuoted = false))
        result ← CompletionResult.of(completions, identifier.region)
      } yield result
    val prioritiseMembers = exprOpt.exists {
      case memberExpr: MemberExpr ⇒ shouldPrioritise(memberExpr)
      case _                      ⇒ false
    }
    MemberCompletionResult(exprOpt.isDefined, completionResultOpt, prioritiseMembers)
  }

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

  private def findMemberExprOrImport(text: String, identifier: Token, parser: CompletionParser): Option[Expr] = {
    val expr = parser.parse(text)
    for {
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.node.tokens
      memberExpr ← findMemberExprOrImport(expr, identifier)
    } yield memberExpr
  }

  private def findMemberExprOrImport(expr: Expr, token: Token): Option[Expr] = expr.find {
    case e@MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.MemberExpr(_, _, `token`))))           ⇒ e
    case e@MemberExpr(_, _, _, Some(SourceInfo(_, ConcreteSyntax.HeadlessMemberExpr(_, `token`))))      ⇒ e
    case e@ImportStatement(_, _, Some(SourceInfo(_, ConcreteSyntax.ImportStatement(_, _, _, `token`)))) ⇒ e
  }

  private def getMembers(klass: MashClass): Seq[MemberInfo] = {
    val fieldMembers = klass.fields.map(f ⇒
      MemberInfo(f.name, classNameOpt = klass.nameOpt, descriptionOpt = f.summaryOpt, isField = true))
    val methodMembers = klass.methods.flatMap(m ⇒
      m.names.map(name ⇒ MemberInfo(name, classNameOpt = klass.nameOpt, descriptionOpt = m.summaryOpt, isField = false)))
    val parentClassMembers = klass.parentOpt.toSeq.flatMap(parentClass ⇒ getMembers(parentClass))
    distinct(fieldMembers ++ methodMembers ++ parentClassMembers)
  }

  private def getMembers(userClass: UserClass): Seq[MemberInfo] = {
    val fieldMembers = userClass.params.params.flatMap(_.nameOpt).map(name ⇒
      MemberInfo(name, classNameOpt = Some(userClass.name), isField = true))
    val methodMembers = userClass.methods.collect { case (name, method) if method.isPublic ⇒
      MemberInfo(name, classNameOpt = Some(userClass.name), isField = false)
    }
    val parentClassMembers = getMembers(ObjectClass)
    distinct(fieldMembers ++ methodMembers ++ parentClassMembers)
  }

  private def getStaticMembers(userClass: UserClass): Seq[MemberInfo] = {
    val staticMethodMembers = Seq(MemberInfo("new", classNameOpt = Some(userClass.name), isField = false, isStatic = true))
    distinct(staticMethodMembers ++ getMembers(ClassClass))
  }

  private def getValueMembers(klass: MashClass): Seq[MemberInfo] = {
    val staticMethodMembers = klass.staticMethods.map(method ⇒
      MemberInfo(method.name, classNameOpt = klass.nameOpt, descriptionOpt = method.summaryOpt, isField = false, isStatic = true))
    distinct(staticMethodMembers ++ getMembers(ClassClass))
  }

  private def getValueMembers(obj: MashObject): Seq[MemberInfo] =
    obj.classOpt match {
      case Some(klass) ⇒
        val extraFieldMembers = obj.fields.keys.toSeq.filterNot(klass.fieldsMap.contains).map(MemberInfo(_, isField = true))
        distinct(extraFieldMembers ++ getMembers(klass))
      case None        ⇒
        val fieldMembers = obj.fields.keys.toSeq.map(MemberInfo(_, isField = true))
        distinct(fieldMembers ++ getMembers(ObjectClass))
    }

  private def getValueMembers(value: MashValue): Option[Seq[MemberInfo]] =
    condOpt(value) {
      case klass: MashClass ⇒ getValueMembers(klass)
      case obj: MashObject  ⇒ getValueMembers(obj)
    }

  private def getMembers(type_ : Type,
                         canVectorise: Boolean = true): Seq[MemberInfo] = type_ match {
    case Type.Instance(klass)              ⇒ getMembers(klass)
    case Type.UserClassInstance(userClass) ⇒ getMembers(userClass)
    case userClass: Type.UserClass         ⇒ getStaticMembers(userClass)
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