package com.github.mdr.mash.parser

import com.github.mdr.mash.inference.Type
import scala.collection.immutable.ListMap
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.evaluator.SourceLocation

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
      val withTransformedDescendents =
        this match {
          case Hole(_) | Literal(_, _) | StringLiteral(_, _, _, _) | Identifier(_, _) | MishFunction(_, _) | HeadlessMemberExpr(_, _, _) ⇒
            this
          case InterpolatedString(start, parts, end, sourceInfoOpt) ⇒
            InterpolatedString(start, parts.map {
              case StringPart(s) ⇒ StringPart(s)
              case ExprPart(e)   ⇒ ExprPart(e.transform(f))
            }, end, sourceInfoOpt)
          case AssignmentExpr(left, right, alias, sourceInfoOpt) ⇒
            AssignmentExpr(left.transform(f), right.transform(f), alias, sourceInfoOpt)
          case ParenExpr(expr, sourceInfoOpt) ⇒
            ParenExpr(expr.transform(f), sourceInfoOpt)
          case StatementSeq(statements, sourceInfoOpt) ⇒
            StatementSeq(statements.map(_.transform(f)), sourceInfoOpt)
          case LambdaExpr(parameter, body, sourceInfoOpt) ⇒
            LambdaExpr(parameter, body.transform(f), sourceInfoOpt)
          case PipeExpr(left, right, sourceInfoOpt) ⇒
            PipeExpr(left.transform(f), right.transform(f), sourceInfoOpt)
          case MemberExpr(expr, name, isNullSafe, sourceInfoOpt) ⇒
            MemberExpr(expr.transform(f), name, isNullSafe, sourceInfoOpt)
          case LookupExpr(expr, index, sourceInfoOpt) ⇒
            LookupExpr(expr.transform(f), index.transform(f), sourceInfoOpt)
          case InvocationExpr(function, arguments, isParenInvocation, sourceInfoOpt) ⇒
            val newArguments = arguments.map(_.transform(f).asInstanceOf[Argument])
            InvocationExpr(function.transform(f), newArguments, isParenInvocation, sourceInfoOpt)
          case BinOpExpr(left, op, right, sourceInfoOpt) ⇒
            BinOpExpr(left.transform(f), op, right.transform(f), sourceInfoOpt)
          case ChainedOpExpr(left, opRights, sourceInfoOpt) ⇒
            ChainedOpExpr(left.transform(f), opRights.map { case (op, right) ⇒ op -> right.transform(f) }, sourceInfoOpt)
          case IfExpr(cond, body, elseOpt, sourceInfoOpt) ⇒
            IfExpr(cond.transform(f), body.transform(f), elseOpt.map(_.transform(f)), sourceInfoOpt)
          case ListExpr(items, sourceInfoOpt) ⇒
            ListExpr(items.map(_.transform(f)), sourceInfoOpt)
          case ObjectExpr(entries, sourceInfoOpt) ⇒
            val newEntries = for ((label, value) ← entries) yield (label, value.transform(f))
            ObjectExpr(newEntries, sourceInfoOpt)
          case MinusExpr(expr, sourceInfoOpt) ⇒
            MinusExpr(expr.transform(f), sourceInfoOpt)
          case MishInterpolation(part, sourceInfoOpt) ⇒
            val newPart = part match {
              case StringPart(s) ⇒ StringPart(s)
              case ExprPart(e)   ⇒ ExprPart(e.transform(f))
            }
            MishInterpolation(newPart, sourceInfoOpt)
          case MishExpr(command, args, captureProcessOutput, sourceInfoOpt) ⇒
            MishExpr(command.transform(f), args.map(_.transform(f)), captureProcessOutput, sourceInfoOpt)
          case FunctionDeclaration(name, params, body, sourceInfoOpt) ⇒
            FunctionDeclaration(name, params, body.transform(f), sourceInfoOpt)
          case HelpExpr(expr, sourceInfoOpt) ⇒
            HelpExpr(expr.transform(f), sourceInfoOpt)
          case ExprPart(expr) ⇒
            ExprPart(expr.transform(f))
          case StringPart(_) ⇒
            this
          case SimpleParam(_, _) | VariadicParam(_, _) ⇒
            this
          case Argument.PositionArg(expr, sourceInfoOpt) ⇒
            Argument.PositionArg(expr.transform(f), sourceInfoOpt)
          case Argument.ShortFlag(_, _) ⇒
            this
          case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒
            Argument.LongFlag(flag, valueOpt.map(_.transform(f)), sourceInfoOpt)

        }
      f.lift.apply(withTransformedDescendents).getOrElse(withTransformedDescendents)
    }
  }

  sealed trait Expr extends AstNode {

    override def transform(f: PartialFunction[AstNode, AstNode]): Expr = super.transform(f).asInstanceOf[Expr]

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]): Expr

    var typeOpt: Option[Type] = None

    var typeBindings: Map[String, Type] = Map()

    override def toString: String = PrettyPrinter.pretty(this)

  }

  case class Literal(value: MashValue, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    assert(value == MashNull || value.isInstanceOf[MashBoolean] || value.isInstanceOf[MashNumber])
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class StringLiteral(s: String, quotationType: QuotationType, hasTildePrefix: Boolean = false, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
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

  case class StatementSeq(statements: Seq[Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = statements
  }

  case class LambdaExpr(parameter: String, body: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(body)
  }

  case class BinOpExpr(left: Expr, op: BinaryOperator, right: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
  }

  case class ChainedOpExpr(left: Expr, opRights: Seq[(BinaryOperator, Expr)], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = left +: opRights.map(_._2)
  }

  case class IfExpr(cond: Expr, body: Expr, elseOpt: Option[Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(cond, body) ++ elseOpt.toSeq
  }

  case class ListExpr(items: Seq[Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = items
  }

  case class ObjectExpr(fields: ListMap[String, Expr], sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = fields.values.toSeq
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

  case class AssignmentExpr(left: Expr, right: Expr, alias: Boolean, sourceInfoOpt: Option[SourceInfo]) extends Expr {
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
  case class MishExpr(command: Expr, args: Seq[Expr], captureProcessOutput: Boolean, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = command +: args
  }

  sealed trait FunctionParam extends AstNode {
    val name: String
    def isVariadic: Boolean
  }

  case class SimpleParam(name: String, sourceInfoOpt: Option[SourceInfo]) extends FunctionParam {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
    def isVariadic = false
  }

  case class VariadicParam(name: String, sourceInfoOpt: Option[SourceInfo]) extends FunctionParam {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
    def isVariadic = true
  }

  case class FunctionDeclaration(name: String, params: Seq[FunctionParam], body: Expr, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(body) ++ params
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

}

case class SourceInfo(source: String, expr: ConcreteSyntax.AstNode) {

  def location = SourceLocation(source, expr.pointedRegion)

}

sealed trait QuotationType
object QuotationType {
  case object Double extends QuotationType
  case object Single extends QuotationType
}