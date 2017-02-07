package com.github.mdr.mash.compiler

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.ns.core.{ AnyClass, ObjectClass }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ BinaryOperator, QuotationType }

import scala.collection.mutable.ArrayBuffer

object BareStringify {

  def bareStringify(program: Program, bindings: Set[String]): Program = {
    val context = new BareStringificationContext
    val newBody = context.bareStringify(program.body, bindings)
    program.copy(body = newBody)
  }

  def getBareTokens(expr: Expr, bindings: Set[String]): Set[Token] = {
    val context = new BareStringificationContext
    context.bareStringify(expr, bindings)
    context.bareWords
  }

}

class BareStringificationContext {

  var bareWords: Set[Token] = Set()

  def bareStringify(expr: Expr, bindings: Set[String]): Expr = expr match {
    case Identifier(name, sourceInfoOpt)                                                                                      ⇒
      if (bindings contains name)
        expr
      else {
        for {
          sourceInfo ← sourceInfoOpt
          token ← sourceInfo.node.tokens
        } bareWords += token
        StringLiteral(name, QuotationType.Double, sourceInfoOpt = sourceInfoOpt)
      }
    case Hole(_) | Literal(_, _) | StringLiteral(_, _, _, _) | MishFunction(_, _) | HeadlessMemberExpr(_, _, _) | ThisExpr(_) ⇒
      expr
    case InterpolatedString(start, parts, end, sourceInfoOpt)                                                                 ⇒
      val newParts = parts.map {
        case StringPart(s)  ⇒ StringPart(s)
        case ExprPart(expr) ⇒ ExprPart(bareStringify(expr, bindings))
      }
      InterpolatedString(start, newParts, end, sourceInfoOpt)
    case ParenExpr(expr, sourceInfoOpt)                                                                                       ⇒
      ParenExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case BlockExpr(expr, sourceInfoOpt)                                                                                       ⇒
      BlockExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case StatementSeq(statements, sourceInfoOpt)                                                                              ⇒
      var newStatements = ArrayBuffer[Expr]()
      var latestBindings = bindings
      for (statement ← statements) {
        newStatements += bareStringify(statement, latestBindings)
        latestBindings = latestBindings ++ getNewlyBoundNames(statement)
      }
      StatementSeq(newStatements, sourceInfoOpt)
    case PipeExpr(left, right, sourceInfoOpt)                                                                                 ⇒
      PipeExpr(bareStringify(left, bindings), bareStringify(right, bindings), sourceInfoOpt)
    case MemberExpr(expr, name, isNullSafe, sourceInfoOpt)                                                                    ⇒
      MemberExpr(bareStringify(expr, bindings), name, isNullSafe, sourceInfoOpt)
    case LookupExpr(expr, index, sourceInfoOpt)                                                                               ⇒
      LookupExpr(bareStringify(expr, bindings), bareStringify(index, bindings), sourceInfoOpt)
    case InvocationExpr(function, arguments, isParenInvocation, sourceInfoOpt)                                                ⇒
      val newArguments = arguments.map {
        case Argument.PositionArg(expr, sourceInfoOpt)        ⇒ Argument.PositionArg(bareStringify(expr, bindings), sourceInfoOpt)
        case arg: Argument.ShortFlag                          ⇒ arg
        case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒ Argument.LongFlag(flag, valueOpt.map(bareStringify(_, bindings)), sourceInfoOpt)
      }
      InvocationExpr(bareStringify(function, bindings), newArguments, isParenInvocation, sourceInfoOpt)
    case BinOpExpr(left, op@BinaryOperator.Sequence, right, sourceInfoOpt)                                                    ⇒
      val extraGlobals = getNewlyBoundNames(left)
      val newBindings = bindings ++ extraGlobals
      BinOpExpr(bareStringify(left, bindings), op, bareStringify(right, newBindings), sourceInfoOpt)
    case BinOpExpr(left, op, right, sourceInfoOpt)                                                                            ⇒
      BinOpExpr(bareStringify(left, bindings), op, bareStringify(right, bindings), sourceInfoOpt)
    case ChainedOpExpr(left, opRights, sourceInfoOpt)                                                                         ⇒
      ChainedOpExpr(bareStringify(left, bindings), opRights.map { case (op, right) ⇒ op -> bareStringify(right, bindings) }, sourceInfoOpt)
    case IfExpr(cond, body, elseOpt, sourceInfoOpt)                                                                           ⇒
      IfExpr(bareStringify(cond, bindings), bareStringify(body, bindings), elseOpt.map(bareStringify(_, bindings)), sourceInfoOpt)
    case ListExpr(elements, sourceInfoOpt)                                                                                    ⇒
      ListExpr(elements.map(bareStringify(_, bindings)), sourceInfoOpt)
    case ObjectExpr(entries, sourceInfoOpt)                                                                                   ⇒
      val newEntries =
        entries.map {
          case FullObjectEntry(ident@Identifier(label, _), value, sourceInfoOpt) ⇒
            FullObjectEntry(ident, bareStringify(value, bindings), sourceInfoOpt)
          case FullObjectEntry(label, value, sourceInfoOpt)                      ⇒
            FullObjectEntry(bareStringify(label, bindings), bareStringify(value, bindings), sourceInfoOpt)
          case ShorthandObjectEntry(field, sourceInfoOpt)                        ⇒
            ShorthandObjectEntry(field, sourceInfoOpt)
        }
      ObjectExpr(newEntries, sourceInfoOpt)
    case MinusExpr(expr, sourceInfoOpt)                                                                                       ⇒
      MinusExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case AssignmentExpr(left@Identifier(name, _), operatorOpt, right, sourceInfoOpt)                                          ⇒
      AssignmentExpr(left, operatorOpt, bareStringify(right, bindings), sourceInfoOpt)
    case AssignmentExpr(left, operatorOpt, right, sourceInfoOpt)                                                              ⇒
      AssignmentExpr(bareStringify(left, bindings), operatorOpt, bareStringify(right, bindings), sourceInfoOpt)
    case PatternAssignmentExpr(pattern, right, sourceInfoOpt)                                                                 ⇒
      PatternAssignmentExpr(pattern, bareStringify(right, bindings), sourceInfoOpt)
    case MishExpr(command, args, redirects, captureProcessOutput, sourceInfoOpt)                                              ⇒
      val newRedirects = redirects.map {
        case MishRedirect(op, arg, sourceInfoOpt) ⇒ MishRedirect(op, bareStringify(arg, bindings), sourceInfoOpt)
      }
      val newArgs = args.map(bareStringify(_, bindings))
      MishExpr(bareStringify(command, bindings), newArgs, newRedirects, captureProcessOutput, sourceInfoOpt)
    case MishInterpolation(part, sourceInfoOpt)                                                                               ⇒
      val newPart = part match {
        case StringPart(s)  ⇒ StringPart(s)
        case ExprPart(expr) ⇒ ExprPart(bareStringify(expr, bindings))
      }
      MishInterpolation(newPart, sourceInfoOpt)
    case LambdaExpr(params, body, sourceInfoOpt)                                                                              ⇒
      LambdaExpr(bareStringify(params, bindings), bareStringify(body, bindings ++ params.boundNames), sourceInfoOpt)
    case FunctionDeclaration(docCommentOpt, attributes, name, params, body, sourceInfoOpt)                                   ⇒
      val newBody = bareStringify(body, bindings ++ params.boundNames + name)
      FunctionDeclaration(docCommentOpt, attributes, name, bareStringify(params, bindings), newBody, sourceInfoOpt)
    case ClassDeclaration(docCommentOpt, attributes, name, params, bodyOpt, sourceInfoOpt)                                    ⇒
      val newBody = bodyOpt.map(bareStringify(_, bindings ++ params.boundNames))
      ClassDeclaration(docCommentOpt, attributes, name, bareStringify(params, bindings), newBody, sourceInfoOpt)
    case HelpExpr(expr, sourceInfoOpt)                                                                                        ⇒
      HelpExpr(bareStringify(expr, bindings), sourceInfoOpt)
  }

  private def bareStringify(param: FunctionParam, bindings: Set[String]): FunctionParam =
    param.copy(defaultExprOpt = param.defaultExprOpt.map(bareStringify(_, bindings)))

  private def bareStringify(params: ParamList, bindings: Set[String]): ParamList =
    ParamList(params.params.map(bareStringify(_, bindings)))

  private def bareStringify(body: ClassBody, bindings: Set[String]): ClassBody = {
    val methodNames = body.methods.map(_.name).toSet
    val parentMethodNames = ObjectClass.methodNames.toSet ++ AnyClass.methodNames.toSet
    val bodyBindings = bindings ++ methodNames ++ parentMethodNames
    val methods = body.methods.map(method ⇒ bareStringify(method, bodyBindings).asInstanceOf[FunctionDeclaration])
    ClassBody(methods, body.sourceInfoOpt)
  }

  def getNewlyBoundNames(left: Expr): Seq[String] =
    left.findAll {
      case AssignmentExpr(left@Identifier(name, _), _, _, _) ⇒ Seq(name)
      case PatternAssignmentExpr(pattern, _, _)              ⇒ pattern.boundNames
      case FunctionDeclaration(_, _, name, _, _, _)          ⇒ Seq(name)
      case ClassDeclaration(_, _, name, _, _, _)             ⇒ Seq(name)
    }.flatten
}