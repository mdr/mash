package com.github.mdr.mash.parser

import com.github.mdr.mash.evaluator.{ Attributes, SourceLocation }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime._

/**
  * Trees representing the abstract syntax of mash, without any unnecessary lexical information
  *
  * See also: ConcreteSyntax (which retains the lexical information)
  */
object AbstractSyntax {

  sealed trait AstNode extends AstNodeTransformation {

    val sourceInfoOpt: Option[SourceInfo]

    def locationOpt: Option[SourceLocation] = sourceInfoOpt.map(_.location)

    def children: Seq[AstNode]

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]): AstNode

    def find[T](f: PartialFunction[AstNode, T]): Option[T] =
      children.flatMap(_.find(f)).headOption orElse f.lift(this)

    def findAll[T](f: PartialFunction[AstNode, T]): Seq[T] =
      children.flatMap(_.find(f)) ++ f.lift(this)

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

  case class ObjectPatternEntry(field: String,
                                valuePatternOpt: Option[Pattern],
                                sourceInfoOpt: Option[SourceInfo]) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = valuePatternOpt.toSeq

    def boundNames = valuePattern.boundNames

    val valuePattern = valuePatternOpt getOrElse IdentPattern(field)
  }

  case class ObjectPattern(entries: Seq[ObjectPatternEntry], sourceInfoOpt: Option[SourceInfo] = None) extends Pattern {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = entries

    def boundNames: Seq[String] = entries.flatMap(_.boundNames)
  }

  case class ListPattern(patterns: Seq[Pattern], sourceInfoOpt: Option[SourceInfo] = None) extends Pattern {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = patterns

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

  case class InterpolatedString(start: String,
                                parts: Seq[InterpolationPart],
                                end: String,
                                sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
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
  case class MemberExpr(target: Expr,
                        name: String,
                        isNullSafe: Boolean,
                        sourceInfoOpt: Option[SourceInfo]) extends AbstractMemberExpr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(target)
  }

  /**
    * An expression of the form: .member or ?.member
    */
  case class HeadlessMemberExpr(name: String,
                                isNullSafe: Boolean,
                                sourceInfoOpt: Option[SourceInfo]) extends AbstractMemberExpr {
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

  case class ChainedOpExpr(left: Expr,
                           opRights: Seq[(BinaryOperator, Expr)],
                           sourceInfoOpt: Option[SourceInfo]) extends Expr {
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
  case class MishExpr(command: Expr, args: Seq[Expr],
                      redirects: Seq[MishRedirect],
                      captureProcessOutput: Boolean,
                      sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children: Seq[Expr] = command +: args
  }

  case class MishRedirect(operator: RedirectOperator,
                          arg: Expr,
                          sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(arg)
  }

  case class FunctionParam(attributes: Seq[Attribute] = Seq(),
                           nameOpt: Option[String],
                           isVariadic: Boolean = false,
                           defaultExprOpt: Option[Expr] = None,
                           patternOpt: Option[Pattern] = None,
                           sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = attributes ++ defaultExprOpt ++ patternOpt

    def boundNames: Seq[String] = (nameOpt.toSeq ++ patternOpt.toSeq.flatMap(_.boundNames)).distinct

  }

  case class ParamList(params: Seq[FunctionParam]) extends AstNode {

    def children = params

    val sourceInfoOpt = None

    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = this

    def boundNames = params.flatMap(_.boundNames)
  }

  case class Attribute(name: String,
                       argumentsOpt: Option[Seq[Argument]] = None,
                       sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = argumentsOpt.toSeq.flatten
  }

  case class FunctionDeclaration(docCommentOpt: Option[DocComment],
                                 attributes: Seq[Attribute],
                                 name: String,
                                 params: ParamList,
                                 body: Expr,
                                 sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(params, body)
  }

  case class ClassDeclaration(docCommentOpt: Option[DocComment],
                              attributes: Seq[Attribute],
                              name: String,
                              params: ParamList,
                              bodyOpt: Option[ClassBody],
                              sourceInfoOpt: Option[SourceInfo] = None) extends Expr {
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
    * readlines?
    */
  case class HelpExpr(expr: Expr, sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq(expr)
  }

  case class ThisExpr(sourceInfoOpt: Option[SourceInfo]) extends Expr {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq()
  }

  /**
    * namespace foo.bar.baz
    */
  case class NamespaceDeclaration(segments: Seq[String], sourceInfoOpt: Option[SourceInfo]) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = Seq()
  }

  case class Program(namespaceOpt: Option[NamespaceDeclaration],
                     body: Expr,
                     sourceInfoOpt: Option[SourceInfo] = None) extends AstNode {
    def withSourceInfoOpt(sourceInfoOpt: Option[SourceInfo]) = copy(sourceInfoOpt = sourceInfoOpt)

    def children = namespaceOpt.toSeq :+ body
  }

}