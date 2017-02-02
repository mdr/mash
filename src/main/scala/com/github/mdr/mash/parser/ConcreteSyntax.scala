package com.github.mdr.mash.parser

import com.github.mdr.mash.utils.{ PointedRegion, Region }
import com.github.mdr.mash.lexer.{ Token, TokenType, DocComment ⇒ LexerDocComment }

import scala.language.implicitConversions

/**
  * Trees representing the concrete syntax of mash (retaining all the semantically uninteresting tokens)
  */
object ConcreteSyntax {

  sealed trait AstNode {

    val tokens: Seq[Token]

    def startPos: Int = tokens.head.offset

    def posAfter: Int = tokens.last.region.posAfter

    def region = Region(startPos, posAfter - startPos)

    def pointedRegion = PointedRegion(startPos, region)

    def children: Seq[AstNode]

    def find[T](f: PartialFunction[AstNode, T]): Option[T] =
      children.flatMap(_.find(f)).headOption orElse f.lift(this)

    def findAll[T](f: PartialFunction[AstNode, T]): Seq[T] =
      children.flatMap(_.find(f)) ++ f.lift(this)

  }

  sealed trait Expr extends AstNode

  case class Literal(token: Token) extends Expr {
    require(token.isLiteral)
    val tokens = Seq(token)

    def children = Seq()
  }

  case class ThisExpr(token: Token) extends Expr {
    require(token.tokenType == TokenType.THIS)
    val tokens = Seq(token)

    def children = Seq()
  }

  case class Identifier(token: Token) extends Expr {
    val tokens = Seq(token)

    def children = Seq()
  }

  sealed trait InterpolationPart extends AstNode

  case class SimpleInterpolation(interpolationStart: Token, expr: Expr) extends InterpolationPart {
    lazy val tokens = interpolationStart +: expr.tokens

    def children = Seq(expr)
  }

  case class ComplexInterpolation(interpolationStart: Token, expr: Expr, rbrace: Token) extends InterpolationPart {
    lazy val tokens = interpolationStart +: expr.tokens :+ rbrace

    def children = Seq(expr)
  }

  case class StringPart(stringMiddle: Token) extends InterpolationPart {
    lazy val tokens = Seq(stringMiddle)

    def children = Seq()
  }

  case class InterpolatedString(start: Token, parts: Seq[InterpolationPart], end: Token) extends Expr {
    lazy val tokens = start +: parts.flatMap(_.tokens) :+ end

    def children = parts
  }

  case class Hole(token: Token) extends Expr {
    val tokens = Seq(token)

    def children = Seq()
  }

  /**
    * a = b, a += b, a -= b, a *= b, a /= b
    */
  case class AssignmentExpr(left: Expr, equals: Token, right: Expr) extends Expr {
    lazy val tokens = (left.tokens :+ equals) ++ right.tokens

    def children = Seq(left, right)
  }

  /**
    * { a, b } = someObject
    */
  case class PatternAssignmentExpr(left: Pattern, equals: Token, right: Expr) extends Expr {
    lazy val tokens = (left.tokens :+ equals) ++ right.tokens

    def children = Seq(left, right)

  }

  case class ParenExpr(lparen: Token, expr: Expr, rparen: Token) extends Expr {
    lazy val tokens = lparen +: expr.tokens :+ rparen

    def children = Seq(expr)
  }

  case class Statement(statementOpt: Option[Expr], semiOpt: Option[Token]) extends AstNode {
    lazy val tokens = statementOpt.toSeq.flatMap(_.tokens) ++ semiOpt.toSeq

    def children = statementOpt.toSeq
  }

  case class StatementSeq(statements: Seq[Statement]) extends Expr {
    lazy val tokens = statements.flatMap(_.tokens)

    def children = statements
  }

  case class BlockExpr(lbrace: Token, statements: Expr, rbrace: Token) extends Expr {
    lazy val tokens = lbrace +: statements.tokens :+ rbrace

    def children = Seq(statements)
  }

  case class MemberExpr(expr: Expr, dot: Token, name: Token) extends Expr {
    require(dot.tokenType == TokenType.DOT || dot.tokenType == TokenType.DOT_NULL_SAFE)

    def isNullSafe = dot.tokenType == TokenType.DOT_NULL_SAFE

    lazy val tokens = expr.tokens :+ dot :+ name

    def children = Seq(expr)

  }

  case class HeadlessMemberExpr(dot: Token, name: Token) extends Expr {
    require(dot.tokenType == TokenType.DOT || dot.tokenType == TokenType.DOT_NULL_SAFE)

    def isNullSafe = dot.tokenType == TokenType.DOT_NULL_SAFE

    lazy val tokens = Seq(dot, name)

    def children = Seq()
  }

  case class LookupExpr(expr: Expr, lsquare: Token, indexExpr: Expr, rsquare: Token) extends Expr {
    lazy val tokens = (expr.tokens :+ lsquare) ++ indexExpr.tokens :+ rsquare

    def children = Seq(expr, indexExpr)
  }

  /**
    * args can be Expr's, for position arguments, or Long/ShortArgs
    */
  case class InvocationExpr(function: Expr, args: Seq[AstNode]) extends Expr {
    lazy val tokens = function.tokens ++ args.flatMap(_.tokens)

    def children = function +: args
  }

  case class ParenInvocationArgs(firstArg: Expr, otherArgs: Seq[(Token, Expr)]) extends AstNode {
    lazy val tokens = firstArg.tokens ++ otherArgs.flatMap { case (comma, arg) ⇒ comma +: arg.tokens }

    def children = firstArg +: otherArgs.map(_._2)
  }

  case class ParenInvocationExpr(function: Expr, lparen: Token, argsOpt: Option[ParenInvocationArgs], rparen: Token) extends Expr {
    lazy val tokens = (function.tokens :+ lparen) ++ argsOpt.toSeq.flatMap(_.tokens) :+ rparen

    def children = function +: argsOpt.toSeq
  }

  case class LambdaExpr(params: ParamList, arrow: Token, body: Expr) extends Expr {
    lazy val tokens = (params.tokens :+ arrow) ++ body.tokens

    def children = Seq(params, body)
  }

  case class PipeExpr(left: Expr, pipe: Token, right: Expr) extends Expr {
    lazy val tokens = (left.tokens :+ pipe) ++ right.tokens

    def children = Seq(left, right)
  }

  case class BinOpExpr(left: Expr, op: Token, right: Expr) extends Expr {
    lazy val tokens = (left.tokens :+ op) ++ right.tokens

    override def pointedRegion = PointedRegion(op.offset, region)

    def children = Seq(left, right)
  }

  /**
    * e.g. 0 <= x < y <= 100
    */
  case class ChainedOpExpr(left: Expr, opRights: Seq[(Token, Expr)]) extends Expr {
    lazy val tokens = left.tokens ++ opRights.flatMap { case (token, expr) ⇒ token +: expr.tokens }

    def children = left +: opRights.map(_._2)
  }

  case class IfExpr(ifToken: Token, cond: Expr, thenToken: Token, body: Expr, elseOpt: Option[(Token, Expr)]) extends Expr {
    lazy val tokens = (ifToken +: cond.tokens :+ thenToken) ++ body.tokens ++
      elseOpt.map { case (token, expr) ⇒ token +: expr.tokens }.getOrElse(Seq())

    def children = Seq(cond, body) ++ elseOpt.map(_._2)

  }

  case class ListExpr(lsquare: Token, contentsOpt: Option[ListExprContents], rsquare: Token) extends Expr {
    lazy val tokens = lsquare +: contentsOpt.toSeq.flatMap(_.tokens) :+ rsquare

    def children = contentsOpt.toSeq
  }

  case class ListExprContents(firstElement: Expr, otherElements: Seq[(Token, Expr)]) extends AstNode {
    lazy val tokens = firstElement.tokens ++ otherElements.flatMap { case (comma, element) ⇒ comma +: element.tokens }

    def children = firstElement +: otherElements.map(_._2)

  }

  sealed trait ObjectEntry extends AstNode

  case class ShorthandObjectEntry(identifier: Token) extends ObjectEntry {
    lazy val tokens = Seq(identifier)

    def children = Seq()
  }

  case class FullObjectEntry(field: Expr, colon: Token, value: Expr) extends ObjectEntry {
    lazy val tokens = field.tokens ++ (colon +: value.tokens)

    def children = Seq(field, value)
  }

  case class ObjectExpr(lbrace: Token, contentsOpt: Option[ObjectExprContents], rbrace: Token) extends Expr {
    lazy val tokens = lbrace +: contentsOpt.toSeq.flatMap(_.tokens) :+ rbrace

    def children = contentsOpt.toSeq
  }

  case class ObjectExprContents(firstEntry: ObjectEntry, otherEntries: Seq[(Token, ObjectEntry)]) extends AstNode {
    lazy val tokens = firstEntry.tokens ++ otherEntries.flatMap { case (colon, entry) ⇒ colon +: entry.tokens }

    def children = firstEntry +: otherEntries.map(_._2)
  }

  case class MinusExpr(minus: Token, expr: Expr) extends Expr {
    lazy val tokens = minus +: expr.tokens

    def children = Seq(expr)
  }

  /**
    * e.g -r
    */
  case class ShortArg(flag: Token) extends AstNode {
    lazy val tokens = Seq(flag)

    def children = Seq()
  }

  /**
    * e.g.
    * --recursive
    * --foo=bar
    */
  case class LongArg(flag: Token, equalsValueOpt: Option[(Token, Expr)] = None) extends AstNode {
    lazy val tokens = Seq(flag) ++ equalsValueOpt.toSeq.flatMap { case (token, expr) ⇒ token +: expr.tokens }

    def children = equalsValueOpt.map(_._2).toSeq
  }

  /**
    * Mish item (command or argument), either a bare word, a quoted string, or a Mash interpolation
    */
  sealed trait MishItem extends AstNode

  case class MishWord(token: Token) extends MishItem {
    lazy val tokens = Seq(token)

    def children = Seq()
  }

  case class MishString(expr: Expr) extends MishItem {
    lazy val tokens = expr.tokens

    def children = Seq(expr)
  }

  case class MishInterpolation(part: InterpolationPart) extends MishItem {
    lazy val tokens = part.tokens

    def children = Seq(part)
  }

  case class MishRedirect(redirectToken: Token, item: MishItem) extends MishItem {
    lazy val tokens = redirectToken +: item.tokens

    def children = Seq(item)
  }

  /**
    * Raw mish command of the form: cmd arg1 arg2 arg3
    */
  case class MishExpr(command: MishItem, args: Seq[MishItem]) extends Expr {
    lazy val tokens = command.tokens ++ args.flatMap(_.tokens)

    def children = command +: args
  }

  /**
    * Mish expression inside Mash, of the form !{ ... }
    */
  case class MishInterpolationExpr(start: Token, expr: MishExpr, rbrace: Token) extends Expr {
    lazy val tokens = (start +: expr.tokens) :+ rbrace

    def children = Seq(expr)
  }

  sealed trait Pattern extends AstNode {
    def nameOpt: Option[String] = None
  }

  case class IdentPattern(identifier: Token) extends Pattern {
    lazy val tokens = Seq(identifier)

    override def nameOpt = Some(identifier.text)

    def children = Seq()
  }

  case class ObjectPattern(lbrace: Token, contentsOpt: Option[ObjectPatternContents], rbrace: Token) extends Pattern {
    lazy val tokens = lbrace +: contentsOpt.toSeq.flatMap(_.tokens) :+ rbrace

    def children = contentsOpt.toSeq
  }

  case class ObjectPatternContents(firstEntry: ObjectPatternEntry, otherEntries: Seq[(Token, ObjectPatternEntry)]) extends AstNode {
    lazy val tokens = firstEntry.tokens ++ otherEntries.flatMap { case (comma, entry) ⇒ comma +: entry.tokens }

    def children = firstEntry +: otherEntries.map(_._2)
  }

  sealed trait ObjectPatternEntry extends AstNode

  /**
    * { ..., foo, ... }
    */
  case class ShorthandObjectPatternEntry(identifier: Token) extends ObjectPatternEntry {
    lazy val tokens = Seq(identifier)

    def children = Seq()
  }

  /**
    * { ..., foo: foobar, ... }
    */
  case class FullObjectPatternEntry(field: Token, colon: Token, value: Pattern) extends ObjectPatternEntry {
    lazy val tokens = field +: (colon +: value.tokens)

    def children = Seq(value)
  }

  case class ListPattern(lsquare: Token, contentsOpt: Option[ListPatternContents], rsquare: Token) extends Pattern {
    lazy val tokens = lsquare +: contentsOpt.toSeq.flatMap(_.tokens) :+ rsquare

    def children = contentsOpt.toSeq
  }

  case class ListPatternContents(firstElement: Pattern, otherElements: Seq[(Token, Pattern)]) extends AstNode {
    lazy val tokens = firstElement.tokens ++ otherElements.flatMap { case (comma, element) ⇒ comma +: element.tokens }

    def children = firstElement +: otherElements.map(_._2)
  }

  case class HolePattern(hole: Token) extends Pattern {
    lazy val tokens = Seq(hole)

    def children = Seq()
  }

  sealed trait Param extends AstNode

  case class SimpleParam(pattern: Pattern,
                         ellipsisOpt: Option[Token] = None) extends Param {

    lazy val tokens = pattern.tokens ++ ellipsisOpt

    def children = Seq(pattern)
  }

  case class ParenParam(lparen: Token,
                        attributesOpt: Option[Attributes],
                        param: SimpleParam,
                        equalsDefaultOpt: Option[(Token, Expr)] = None,
                        rparen: Token) extends Param {

    lazy val tokens = lparen +: (attributesOpt.toSeq.flatMap(_.tokens) ++ param.tokens ++
      equalsDefaultOpt.toSeq.flatMap { case (equals, default) ⇒ equals +: default.tokens}) :+ rparen

    def children = Seq(param) ++ attributesOpt ++ equalsDefaultOpt.map(_._2)
  }

  case class ParamList(params: Seq[Param]) extends AstNode {
    lazy val tokens = params.flatMap(_.tokens)

    def children = params
  }

  case class ClassDeclaration(docCommentOpt: Option[LexerDocComment],
                              classToken: Token,
                              name: Token,
                              params: ParamList,
                              bodyOpt: Option[ClassBody]) extends Expr {
    lazy val tokens = Seq(classToken, name) ++ params.tokens ++ bodyOpt.toSeq.flatMap(_.tokens)

    def children = params +: bodyOpt.toSeq
  }

  case class ClassBody(lbrace: Token, methods: Seq[Method], rbrace: Token) extends AstNode {
    lazy val tokens = lbrace +: methods.flatMap(_.tokens) :+ rbrace

    def children = methods
  }

  case class Method(methodDeclaration: FunctionDeclaration, semiOpt: Option[Token]) extends AstNode {
    lazy val tokens = methodDeclaration.tokens ++ semiOpt

    def children = Seq(methodDeclaration)
  }

  case class Attribute(at: Token, name: Token) extends AstNode {
    lazy val tokens = Seq(at, name)

    def children = Seq()
  }

  case class Attributes(attributes: Seq[Attribute]) extends AstNode {
    lazy val tokens = attributes.flatMap(_.tokens)

    def children = attributes
  }

  case class FunctionDeclaration(docCommentOpt: Option[LexerDocComment],
                                 attributesOpt: Option[Attributes],
                                 defToken: Token,
                                 name: Token,
                                 params: ParamList,
                                 equals: Token,
                                 body: Expr) extends Expr {

    lazy val tokens = attributesOpt.toSeq.flatMap(_.tokens) ++ Seq(defToken, name) ++ params.tokens ++ Seq(equals) ++ body.tokens

    def children = attributesOpt.toSeq ++ Seq(params, body)

  }

  /**
    * An expression of the form: !less
    */
  case class MishFunction(word: Token) extends Expr {
    lazy val tokens = Seq(word)

    def children = Seq()
  }

  case class HelpExpr(expr: Expr, question: Token) extends Expr {
    lazy val tokens = expr.tokens :+ question

    def children = Seq(expr)
  }

  case class NamespaceDeclaration(namespace: Token, firstSegment: Token, dotSegments: Seq[(Token, Token)]) extends AstNode {
    lazy val tokens = Seq(namespace, firstSegment) ++ dotSegments.flatMap { case (dot, segment) ⇒ Seq(dot, segment) }

    def children = Seq()
  }

  case class Program(namespaceOpt: Option[NamespaceDeclaration], body: Expr) extends AstNode {
    lazy val tokens = namespaceOpt.toSeq.flatMap(_.tokens) ++ body.tokens

    def children = Seq(body)
  }

}
