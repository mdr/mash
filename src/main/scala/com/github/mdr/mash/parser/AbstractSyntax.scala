package com.github.mdr.mash.parser

import com.github.mdr.mash.inference.Type
import scala.collection.immutable.ListMap
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.evaluator.MashNumber

/**
 * Trees representing the abstract syntax of mash, losing much of the lexical information.
 *
 * See: ConcreteSyntax
 */
object AbstractSyntax {

  sealed trait Expr {

    val sourceInfoOpt: Option[SourceInfo]

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]): Expr

    var typeOpt: Option[Type] = None

    var typeBindings: Map[String, Type] = Map()

    override def toString: String = PrettyPrinter.pretty(this)

    def transform(f: PartialFunction[Expr, Expr]): Expr = {
      val newExpr =
        this match {
          case Hole(_) | Literal(_, _) | StringLiteral(_, _, _, _) | Identifier(_, _) | MishFunction(_, _) ⇒
            this
          case InterpolatedString(start, parts, end, sourceInfoOpt) ⇒
            InterpolatedString(start, parts.map {
              case StringPart(s) ⇒ StringPart(s)
              case ExprPart(e)   ⇒ ExprPart(e.transform(f))
            }, end, sourceInfoOpt)
          case AssignmentExpr(left, right, sourceInfoOpt) ⇒
            AssignmentExpr(left.transform(f), right.transform(f), sourceInfoOpt)
          case ParenExpr(expr, sourceInfoOpt) ⇒
            ParenExpr(expr.transform(f), sourceInfoOpt)
          case LambdaExpr(parameter, body, sourceInfoOpt) ⇒
            LambdaExpr(parameter, body.transform(f), sourceInfoOpt)
          case PipeExpr(left, right, sourceInfoOpt) ⇒
            PipeExpr(left.transform(f), right.transform(f), sourceInfoOpt)
          case MemberExpr(expr, name, isNullSafe, sourceInfoOpt) ⇒
            MemberExpr(expr.transform(f), name, isNullSafe, sourceInfoOpt)
          case LookupExpr(expr, index, sourceInfoOpt) ⇒
            LookupExpr(expr.transform(f), index.transform(f), sourceInfoOpt)
          case InvocationExpr(function, arguments, sourceInfoOpt) ⇒
            val newArguments = arguments.map {
              case Argument.PositionArg(expr)        ⇒ Argument.PositionArg(expr.transform(f))
              case arg @ Argument.ShortFlag(_)       ⇒ arg
              case Argument.LongFlag(flag, valueOpt) ⇒ Argument.LongFlag(flag, valueOpt.map(_.transform(f)))
            }
            InvocationExpr(function.transform(f), newArguments, sourceInfoOpt)
          case BinOpExpr(left, op, right, sourceInfoOpt) ⇒
            BinOpExpr(left.transform(f), op, right.transform(f), sourceInfoOpt)
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
          case MishExpr(command, args, sourceInfoOpt) ⇒
            MishExpr(command.transform(f), args.map(_.transform(f)), sourceInfoOpt)
          case FunctionDeclaration(name, params, body, sourceInfoOpt) ⇒
            FunctionDeclaration(name, params, body.transform(f), sourceInfoOpt)
          case HelpExpr(expr, sourceInfoOpt) ⇒
            HelpExpr(expr.transform(f), sourceInfoOpt)
        }
      f.lift.apply(newExpr).getOrElse(newExpr)
    }

    def find[T](f: PartialFunction[Expr, T]): Option[T] =
      children.flatMap(_.find(f)).headOption orElse f.lift(this)

    def findAll[T](f: PartialFunction[Expr, T]): Seq[T] =
      children.flatMap(_.find(f)) ++ f.lift(this)

    def children: Seq[Expr]
  }

  case class Literal(value: Any, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    assert(value == null || value.isInstanceOf[Boolean] || value.isInstanceOf[MashNumber])
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class StringLiteral(s: String, quotationType: QuotationType, tildePrefix: Boolean, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  sealed trait InterpolationPart {
    def children: Seq[Expr]
  }

  case class StringPart(s: String) extends InterpolationPart {
    def children = Seq()
  }

  case class ExprPart(expr: Expr) extends InterpolationPart {
    def children = Seq(expr)
  }

  case class InterpolatedString(start: String, parts: Seq[InterpolationPart], end: String, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = parts.flatMap(_.children)
  }

  case class Identifier(name: String, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class Hole(sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq()
  }

  case class MemberExpr(expr: Expr, name: String, isNullSafe: Boolean, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr)
  }

  case class LookupExpr(expr: Expr, index: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr, index)
  }

  case class PipeExpr(left: Expr, right: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
  }

  case class ParenExpr(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(expr)
  }

  case class LambdaExpr(parameter: String, body: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(body)
  }

  case class BinOpExpr(left: Expr, op: BinaryOperator, right: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(left, right)
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
      sourceInfoOpt: Option[SourceInfo] = None) extends Expr {

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(function) ++ arguments.flatMap(_.children)

  }

  sealed trait Argument {

    def children: Seq[Expr]

  }

  object Argument {

    case class PositionArg(expr: Expr) extends Argument {
      def children = Seq(expr)
    }

    case class LongFlag(flag: String, valueOpt: Option[Expr]) extends Argument {
      def children = valueOpt.toSeq
    }

    case class ShortFlag(flags: Seq[String]) extends Argument {
      def children = Seq()
    }

  }

  case class AssignmentExpr(left: Expr, right: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(left, right)

  }

  case class MishInterpolation(part: InterpolationPart, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = part.children
  }

  case class MishExpr(command: Expr, args: Seq[Expr], sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = command +: args
  }

  sealed trait FunctionParam {
    def children: Seq[Expr]
  }
  case class SimpleParam(name: String) extends FunctionParam {
    def children: Seq[Expr] = Seq()
  }

  case class FunctionDeclaration(name: String, params: Seq[FunctionParam], body: Expr, sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)
    def children = Seq(body)
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

case class SourceInfo(expr: ConcreteSyntax.Expr) {

  def pos = expr match {
    case ConcreteSyntax.BinOpExpr(_, op, _) ⇒ op.offset
    case _                                  ⇒ expr.region.offset
  }

  def location = PointedRegion(pos, expr.region)

}

sealed trait QuotationType
object QuotationType {
  case object Double extends QuotationType
  case object Single extends QuotationType
}