package com.github.mdr.mash.parser

import com.github.mdr.mash.evaluator.SourceLocation
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime._

/**
 * Trees representing the abstract syntax of mash, losing much of the lexical information.
 *
 * See: ConcreteSyntax
 */
object AbstractSyntax {

  sealed trait AstNode {

    val sourceInfoOpt: Option[SourceInfo]

    def locationOpt: Option[SourceLocation] = sourceInfoOpt.map(_.location)

    def children: Seq[AstNode]

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]): AstNode

    def find[T](f: PartialFunction[AstNode, T]): Option[T] =
      children.flatMap(_.find(f)).headOption orElse f.lift(this)

    def findAll[T](f: PartialFunction[AstNode, T]): Seq[T] =
      children.flatMap(_.find(f)) ++ f.lift(this)

    def transform(f: PartialFunction[AstNode, AstNode]): AstNode = {
      val withTransformedDescendents = transformDescendents(f)
      f.lift.apply(withTransformedDescendents).getOrElse(withTransformedDescendents)
    }

    private def transformDescendents(f: PartialFunction[AstNode, AstNode]) = this match {
      case Hole(_) | Literal(_, _) | StringLiteral(_, _, _, _) | Identifier(_, _) | MishFunction(_, _) | HeadlessMemberExpr(_, _, _) | _: ShorthandObjectEntry | ThisExpr(_) ⇒
        this
      case InterpolatedString(start, parts, end, sourceInfoOpt) ⇒
        InterpolatedString(start, parts.map {
          case StringPart(s) ⇒ StringPart(s)
          case ExprPart(e)   ⇒ ExprPart(e.transform(f))
        }, end, sourceInfoOpt)
      case AssignmentExpr(left, operatorOpt, right, alias, sourceInfoOpt) ⇒
        AssignmentExpr(left.transform(f), operatorOpt, right.transform(f), alias, sourceInfoOpt)
      case PatternAssignmentExpr(pattern, right, sourceInfoOpt) ⇒
        PatternAssignmentExpr(pattern.transform(f).asInstanceOf[Pattern], right.transform(f), sourceInfoOpt)
      case ParenExpr(expr, sourceInfoOpt) ⇒
        ParenExpr(expr.transform(f), sourceInfoOpt)
      case BlockExpr(expr, sourceInfoOpt) ⇒
        BlockExpr(expr.transform(f), sourceInfoOpt)
      case StatementSeq(statements, sourceInfoOpt) ⇒
        StatementSeq(statements.map(_.transform(f)), sourceInfoOpt)
      case LambdaExpr(params, body, sourceInfoOpt) ⇒
        LambdaExpr(params.transform(f).asInstanceOf[ParamList], body.transform(f), sourceInfoOpt)
      case PipeExpr(left, right, sourceInfoOpt) ⇒
        PipeExpr(left.transform(f), right.transform(f), sourceInfoOpt)
      case MemberExpr(expr, name, isNullSafe, sourceInfoOpt) ⇒
        MemberExpr(expr.transform(f), name, isNullSafe, sourceInfoOpt)
      case LookupExpr(expr, index, sourceInfoOpt) ⇒
        LookupExpr(expr.transform(f), index.transform(f), sourceInfoOpt)
      case InvocationExpr(function, arguments, isParenInvocation, sourceInfoOpt) ⇒
        val newArguments = arguments.map(_.transform(f).asInstanceOf[Argument])
        InvocationExpr(function.transform(f), newArguments, isParenInvocation, sourceInfoOpt)
      case BinOpExpr(left, op, right, sourceInfoOpt)                               ⇒
        BinOpExpr(left.transform(f), op, right.transform(f), sourceInfoOpt)
      case ChainedOpExpr(left, opRights, sourceInfoOpt)                            ⇒
        ChainedOpExpr(left.transform(f), opRights.map { case (op, right) ⇒ op -> right.transform(f) }, sourceInfoOpt)
      case IfExpr(cond, body, elseOpt, sourceInfoOpt)                              ⇒
        IfExpr(cond.transform(f), body.transform(f), elseOpt.map(_.transform(f)), sourceInfoOpt)
      case ListExpr(elements, sourceInfoOpt)                                          ⇒
        ListExpr(elements.map(_.transform(f)), sourceInfoOpt)
      case FullObjectEntry(field, value, sourceInfoOpt)                            ⇒
        FullObjectEntry(field.transform(f), value.transform(f), sourceInfoOpt)
      case ObjectExpr(entries, sourceInfoOpt)                                      ⇒
        ObjectExpr(entries.map(_.transform(f).asInstanceOf[ObjectEntry]), sourceInfoOpt)
      case MinusExpr(expr, sourceInfoOpt)                                          ⇒
        MinusExpr(expr.transform(f), sourceInfoOpt)
      case MishInterpolation(part, sourceInfoOpt)                                  ⇒
        val newPart = part match {
          case StringPart(s) ⇒ StringPart(s)
          case ExprPart(e)   ⇒ ExprPart(e.transform(f))
        }
        MishInterpolation(newPart, sourceInfoOpt)
      case MishExpr(command, args, redirects, captureProcessOutput, sourceInfoOpt) ⇒
        MishExpr(command.transform(f), args.map(_.transform(f)), redirects.map(_.transform(f).asInstanceOf[MishRedirect]), captureProcessOutput, sourceInfoOpt)
      case MishRedirect(op, arg, sourceInfoOpt) ⇒
        MishRedirect(op, arg.transform(f), sourceInfoOpt)
      case FunctionDeclaration(name, params, body, sourceInfoOpt) ⇒
        FunctionDeclaration(name, params.transform(f).asInstanceOf[ParamList], body.transform(f), sourceInfoOpt)
      case ClassDeclaration(name, params, bodyOpt, sourceInfoOpt) ⇒
        ClassDeclaration(name, params.transform(f).asInstanceOf[ParamList], bodyOpt.map(_.transform(f).asInstanceOf[ClassBody]), sourceInfoOpt)
      case ClassBody(methods, sourceInfoOpt) ⇒
        ClassBody(methods.map(_.transform(f).asInstanceOf[FunctionDeclaration]), sourceInfoOpt)
      case HelpExpr(expr, sourceInfoOpt) ⇒
        HelpExpr(expr.transform(f), sourceInfoOpt)
      case ExprPart(expr) ⇒
        ExprPart(expr.transform(f))
      case StringPart(_) ⇒
        this
      case FunctionParam(name, isVariadic, defaultOpt, isLazy, patternOpt, sourceInfoOpt) ⇒
        FunctionParam(name, isVariadic, defaultOpt.map(_.transform(f)), isLazy, patternOpt, sourceInfoOpt)
      case Argument.PositionArg(expr, sourceInfoOpt)                                      ⇒
        Argument.PositionArg(expr.transform(f), sourceInfoOpt)
      case Argument.ShortFlag(_, _)                                                       ⇒
        this
      case Argument.LongFlag(flag, valueOpt, sourceInfoOpt)                               ⇒
        Argument.LongFlag(flag, valueOpt.map(_.transform(f)), sourceInfoOpt)
      case ParamList(params)                                                              ⇒
        ParamList(params.map(_.transform(f).asInstanceOf[FunctionParam]))
      case HolePattern(_) | IdentPattern(_, _)                                            ⇒
        this
      case ObjectPatternEntry(field, valuePatternOpt, sourceInfoOpt)                      ⇒
        ObjectPatternEntry(field, valuePatternOpt.map(_.transform(f).asInstanceOf[Pattern]), sourceInfoOpt)
      case ObjectPattern(entries, sourceInfoOpt)                                          ⇒
        ObjectPattern(entries.map(_.transform(f).asInstanceOf[ObjectPatternEntry]), sourceInfoOpt)
      case ListPattern(patterns, sourceInfoOpt)                                           ⇒
        ListPattern(patterns.map(_.transform(f).asInstanceOf[Pattern]), sourceInfoOpt)
    }

  }

  sealed trait Expr extends AstNode {

    override def transform(f: PartialFunction[AstNode, AstNode]): Expr = super.transform(f).asInstanceOf[Expr]

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]): Expr

    var typeOpt: Option[Type] = None

    /**
     * The type it had before it was invoked as a nullary function/method
     */
    var preInvocationTypeOpt: Option[Type] = None

    var typeBindings: Map[String, Type] = Map()

    var constantValueOpt: Option[MashValue] = None

    override def toString: String = PrettyPrinter.pretty(this)

  }

  sealed trait Pattern extends AstNode {
    def boundNames: Seq[String]
  }

  case class IdentPattern(identifier: String, sourceInfoOpt: Option[SourceInfo] = None) extends Pattern {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
    def boundNames = Seq(identifier)
  }

  case class ObjectPatternEntry(field: String, valuePatternOpt: Option[Pattern], sourceInfoOpt: Option[SourceInfo]) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = valuePatternOpt.toSeq
    def boundNames = valuePattern.boundNames
    val valuePattern = valuePatternOpt getOrElse IdentPattern(field)
  }

  case class ObjectPattern(entries: Seq[ObjectPatternEntry], sourceInfoOpt: Option[SourceInfo] = None) extends Pattern {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = entries.flatMap(_.children)
    def boundNames: Seq[String] = entries.flatMap(_.boundNames)
  }

  case class ListPattern(patterns: Seq[Pattern], sourceInfoOpt: Option[SourceInfo] = None) extends Pattern {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = patterns.flatMap(_.children)
    def boundNames: Seq[String] = patterns.flatMap(_.boundNames)
  }

  case class HolePattern(sourceInfoOpt: Option[SourceInfo] = None) extends Pattern {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
    def boundNames: Seq[String] = Seq()
  }

  case class Literal(value: MashValue, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    assert(value.isNull || value.isInstanceOf[MashBoolean] || value.isInstanceOf[MashNumber])
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class StringLiteral(s: String,
                           quotationType: QuotationType,
                           hasTildePrefix: Boolean = false,
                           sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  sealed trait InterpolationPart extends AstNode

  case class StringPart(s: String) extends InterpolationPart {
    val sourceInfoOpt = None
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = this
    def children = Seq()
  }

  case class ExprPart(expr: Expr) extends InterpolationPart {
    val sourceInfoOpt = None
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = this
    def children = Seq(expr)
  }

  case class InterpolatedString(start: String, parts: Seq[InterpolationPart], end: String, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = parts
  }

  case class Identifier(name: String, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class Hole(sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  sealed trait AbstractMemberExpr extends Expr {
    val name: String
    val isNullSafe: Boolean
  }

  /**
   * An expression of the form: target.member or target?.member
   */
  case class MemberExpr(target: Expr, name: String, isNullSafe: Boolean, sourceInfoOpt: Option[SourceInfo]) extends AbstractMemberExpr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(target)
  }

  /**
   * An expression of the form: .member or ?.member
   */
  case class HeadlessMemberExpr(name: String, isNullSafe: Boolean, sourceInfoOpt: Option[SourceInfo]) extends AbstractMemberExpr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  /**
   * An expression of the form: target[index]
   */
  case class LookupExpr(target: Expr, index: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(target, index)
  }

  case class PipeExpr(left: Expr, right: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
  }

  case class ParenExpr(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr)
  }

  case class BlockExpr(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr)
  }

  case class StatementSeq(statements: Seq[Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = statements
  }

  case class LambdaExpr(params: ParamList, body: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(params, body)
  }

  case class BinOpExpr(left: Expr, op: BinaryOperator, right: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
  }

  case class ChainedOpExpr(left: Expr, opRights: Seq[(BinaryOperator, Expr)], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children: Seq[Expr] = left +: opRights.map(_._2)
  }

  case class IfExpr(cond: Expr, body: Expr, elseOpt: Option[Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children: Seq[Expr] = Seq(cond, body) ++ elseOpt.toSeq
  }

  case class ListExpr(elements: Seq[Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = elements
  }

  sealed trait ObjectEntry extends AstNode

  case class ShorthandObjectEntry(field: String, sourceInfoOpt: Option[SourceInfo]) extends ObjectEntry {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class FullObjectEntry(field: Expr, value: Expr, sourceInfoOpt: Option[SourceInfo]) extends ObjectEntry {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(field, value)
  }

  case class ObjectExpr(fields: Seq[ObjectEntry], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = fields.flatMap(_.children)
  }

  case class MinusExpr(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr)
  }

  case class InvocationExpr(
      function: Expr,
      arguments: Seq[Argument],
      isParenInvocation: Boolean,
      sourceInfoOpt: Option[SourceInfo] = None) extends Expr {

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(function) ++ arguments

  }

  sealed trait Argument extends AstNode

  object Argument {

    case class PositionArg(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Argument {
      def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
      def children = Seq(expr)
    }

    case class LongFlag(flag: String, valueOpt: Option[Expr], sourceInfoOpt: Option[SourceInfo]) extends Argument {
      def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
      def children = valueOpt.toSeq
    }

    case class ShortFlag(flags: Seq[String], sourceInfoOpt: Option[SourceInfo]) extends Argument {
      def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
      def children = Seq()
    }

  }

  case class AssignmentExpr(left: Expr,
                            operatorOpt: Option[BinaryOperator],
                            right: Expr,
                            alias: Boolean,
                            sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
  }

  case class PatternAssignmentExpr(left: Pattern,
                                   right: Expr,
                                   sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
  }

  case class MishInterpolation(part: InterpolationPart, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = part.children
  }

  /**
   * !{which ls} or !!{nano}
   */
  case class MishExpr(command: Expr, args: Seq[Expr], redirects: Seq[MishRedirect], captureProcessOutput: Boolean, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children: Seq[Expr] = command +: args
  }

  case class MishRedirect(operator: RedirectOperator, arg: Expr, sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(arg)
  }

  case class FunctionParam(nameOpt: Option[String],
                           isVariadic: Boolean = false,
                           defaultExprOpt: Option[Expr] = None,
                           isLazy: Boolean = false,
                           patternOpt: Option[Pattern] = None,
                           sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = defaultExprOpt.toSeq
    def boundNames: Seq[String] = nameOpt.toSeq ++ patternOpt.toSeq.flatMap(_.boundNames)
  }

  case class ParamList(params: Seq[FunctionParam]) extends AstNode {

    def children = params

    val sourceInfoOpt = None

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = this

    def boundNames = params.flatMap(_.boundNames)
  }

  case class FunctionDeclaration(name: String, params: ParamList, body: Expr, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(params, body)
  }

  case class ClassDeclaration(name: String, params: ParamList, bodyOpt: Option[ClassBody], sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(params) ++ bodyOpt
  }

  case class ClassBody(methods: Seq[FunctionDeclaration], sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = methods
  }

  case class MishFunction(command: String, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  /**
   * Request for help on a function/member etc, e.g.
   *   readlines?
   */
  case class HelpExpr(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr)
  }

  case class ThisExpr(sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

}

case class SourceInfo(provenance: Provenance, expr: ConcreteSyntax.AstNode) {

  def location = SourceLocation(provenance, expr.pointedRegion)

}

case class Provenance(name: String, source: String)

sealed trait QuotationType
object QuotationType {
  case object Double extends QuotationType
  case object Single extends QuotationType
}