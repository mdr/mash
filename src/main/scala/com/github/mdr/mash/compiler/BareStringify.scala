package com.github.mdr.mash.compiler

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ BinaryOperator, QuotationType }

import scala.collection.mutable.ArrayBuffer

object BareStringify {

  /**
    * Replace unbound identifiers with string literals
    */
  def bareStringify(program: Program, bindings: Set[String]): Program = {
    val context = new BareStringificationContext
    val newBody = context.bareStringify(program.body, bindings)
    program.copy(body = newBody)
  }

  def getBareTokens(expr: Expr, bindings: Set[String]): Set[Token] = {
    val context = new BareStringificationContext
    context.bareStringify(expr, bindings)
    context.bareWordTokens
  }

}

class BareStringificationContext {

  var bareWordTokens: Set[Token] = Set()

  def bareStringify(expr: Expr, bindings: Set[String]): Expr = expr match {
    case identifier: Identifier                                                            ⇒
      bareStringify(identifier, bindings)
    case Hole(_, _) | Literal(_, _) | StringLiteral(_, _, _, _) | MishFunction(_, _)
         | HeadlessMemberExpr(_, _, _) | ThisExpr(_) | _: NamespaceDeclaration             ⇒
      expr
    case InterpolatedString(start, parts, end, sourceInfoOpt)                              ⇒
      val newParts = parts.map {
        case StringPart(s)  ⇒ StringPart(s)
        case ExprPart(expr) ⇒ ExprPart(bareStringify(expr, bindings))
      }
      InterpolatedString(start, newParts, end, sourceInfoOpt)
    case ParenExpr(expr, sourceInfoOpt)                                                    ⇒
      ParenExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case BlockExpr(expr, sourceInfoOpt)                                                    ⇒
      BlockExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case statementSeq: StatementSeq                                                        ⇒
      bareStringify(statementSeq, bindings)
    case PipeExpr(left, right, sourceInfoOpt)                                              ⇒
      PipeExpr(bareStringify(left, bindings), bareStringify(right, bindings), sourceInfoOpt)
    case MemberExpr(expr, name, isSafe, sourceInfoOpt)                                     ⇒
      MemberExpr(bareStringify(expr, bindings), name, isSafe, sourceInfoOpt)
    case LookupExpr(expr, index, sourceInfoOpt)                                            ⇒
      LookupExpr(bareStringify(expr, bindings), bareStringify(index, bindings), sourceInfoOpt)
    case InvocationExpr(function, arguments, isParenInvocation, sourceInfoOpt)             ⇒
      val newArguments = arguments.map(bareStringify(_, bindings))
      InvocationExpr(bareStringify(function, bindings), newArguments, isParenInvocation, sourceInfoOpt)
    case BinOpExpr(left, op@BinaryOperator.Sequence, right, sourceInfoOpt)                 ⇒
      val extraGlobals = getNewlyBoundNames(left)
      val newBindings = bindings ++ extraGlobals
      BinOpExpr(bareStringify(left, bindings), op, bareStringify(right, newBindings), sourceInfoOpt)
    case BinOpExpr(left, op, right, sourceInfoOpt)                                         ⇒
      BinOpExpr(bareStringify(left, bindings), op, bareStringify(right, bindings), sourceInfoOpt)
    case ChainedOpExpr(left, opRights, sourceInfoOpt)                                      ⇒
      val newOpRights = opRights.map { case (op, right) ⇒ op -> bareStringify(right, bindings) }
      ChainedOpExpr(bareStringify(left, bindings), newOpRights, sourceInfoOpt)
    case IfExpr(cond, body, elseOpt, sourceInfoOpt)                                        ⇒
      IfExpr(bareStringify(cond, bindings), bareStringify(body, bindings), elseOpt.map(bareStringify(_, bindings)),
        sourceInfoOpt)
    case ListExpr(elements, sourceInfoOpt)                                                 ⇒
      ListExpr(elements.map(bareStringify(_, bindings)), sourceInfoOpt)
    case ObjectExpr(entries, sourceInfoOpt)                                                ⇒
      val newEntries = entries.map(bareStringify(_, bindings))
      ObjectExpr(newEntries, sourceInfoOpt)
    case MinusExpr(expr, sourceInfoOpt)                                                    ⇒
      MinusExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case AssignmentExpr(left@Identifier(name, _), operatorOpt, right, sourceInfoOpt)       ⇒
      AssignmentExpr(left, operatorOpt, bareStringify(right, bindings), sourceInfoOpt)
    case AssignmentExpr(left, operatorOpt, right, sourceInfoOpt)                           ⇒
      AssignmentExpr(bareStringify(left, bindings), operatorOpt, bareStringify(right, bindings), sourceInfoOpt)
    case PatternAssignmentExpr(pattern, right, sourceInfoOpt)                              ⇒
      PatternAssignmentExpr(pattern, bareStringify(right, bindings), sourceInfoOpt)
    case MishExpr(command, args, redirects, captureProcessOutput, sourceInfoOpt)           ⇒
      val newRedirects = redirects.map {
        case MishRedirect(op, arg, sourceInfoOpt) ⇒ MishRedirect(op, bareStringify(arg, bindings), sourceInfoOpt)
      }
      val newArgs = args.map(bareStringify(_, bindings))
      MishExpr(bareStringify(command, bindings), newArgs, newRedirects, captureProcessOutput, sourceInfoOpt)
    case MishInterpolation(part, sourceInfoOpt)                                            ⇒
      val newPart = part match {
        case StringPart(s)  ⇒ StringPart(s)
        case ExprPart(expr) ⇒ ExprPart(bareStringify(expr, bindings))
      }
      MishInterpolation(newPart, sourceInfoOpt)
    case LambdaExpr(params, body, sourceInfoOpt)                                           ⇒
      LambdaExpr(bareStringify(params, bindings), bareStringify(body, bindings ++ params.boundNames), sourceInfoOpt)
    case FunctionDeclaration(docCommentOpt, attributes, name, params, body, sourceInfoOpt) ⇒
      val newAttributes = attributes.map(bareStringify(_, bindings))
      val newBody = bareStringify(body, bindings ++ params.boundNames + name)
      FunctionDeclaration(docCommentOpt, newAttributes, name, bareStringify(params, bindings), newBody, sourceInfoOpt)
    case ClassDeclaration(docCommentOpt, attributes, name, params, bodyOpt, sourceInfoOpt) ⇒
      val newAttributes = attributes.map(bareStringify(_, bindings))
      val newBody = bodyOpt.map(bareStringify(_, bindings ++ params.boundNames))
      ClassDeclaration(docCommentOpt, newAttributes, name, bareStringify(params, bindings), newBody, sourceInfoOpt)
    case HelpExpr(expr, sourceInfoOpt)                                                     ⇒
      HelpExpr(bareStringify(expr, bindings), sourceInfoOpt)
    case ImportStatement(expr, importNameOpt, sourceInfoOpt)                                            ⇒
      ImportStatement(bareStringify(expr, bindings), importNameOpt, sourceInfoOpt)
  }

  private def bareStringify(attribute: Attribute, bindings: Set[String]): Attribute = {
    val newArgumentsOpt = attribute.argumentsOpt.map(_.map(bareStringify(_, bindings)))
    Attribute(attribute.name, newArgumentsOpt, attribute.sourceInfoOpt)
  }

  private def bareStringify(argument: Argument, bindings: Set[String]): Argument = argument match {
    case Argument.PositionArg(expr, sourceInfoOpt)        ⇒
      Argument.PositionArg(bareStringify(expr, bindings), sourceInfoOpt)
    case arg: Argument.ShortFlag                          ⇒
      arg
    case Argument.LongFlag(flag, valueOpt, sourceInfoOpt) ⇒
      Argument.LongFlag(flag, valueOpt.map(bareStringify(_, bindings)), sourceInfoOpt)
  }

  private def bareStringify(param: FunctionParam, bindings: Set[String]): FunctionParam = {
    val newDefaultExprOpt = param.defaultExprOpt.map(bareStringify(_, bindings))
    val newAttributes = param.attributes.map(bareStringify(_, bindings))
    param.copy(attributes = newAttributes, defaultExprOpt = newDefaultExprOpt)
  }

  private def bareStringify(params: ParamList, bindings: Set[String]): ParamList =
    ParamList(params.params.map(bareStringify(_, bindings)))

  private def bareStringify(body: ClassBody, bindings: Set[String]): ClassBody = {
    val methodNames = body.methods.map(_.name).toSet
    val bodyBindings = bindings ++ methodNames
    val methods = body.methods.map(method ⇒ bareStringify(method, bodyBindings).asInstanceOf[FunctionDeclaration])
    ClassBody(methods, body.sourceInfoOpt)
  }

  private def bareStringify(entry: ObjectEntry, bindings: Set[String]): ObjectEntry = entry match {
    case FullObjectEntry(ident@Identifier(label, _), value, sourceInfoOpt) ⇒
      FullObjectEntry(ident, bareStringify(value, bindings), sourceInfoOpt)
    case FullObjectEntry(label, value, sourceInfoOpt)                      ⇒
      FullObjectEntry(bareStringify(label, bindings), bareStringify(value, bindings), sourceInfoOpt)
    case ShorthandObjectEntry(field, sourceInfoOpt)                        ⇒
      ShorthandObjectEntry(field, sourceInfoOpt)
  }

  private def bareStringify(identifier: Identifier, bindings: Set[String]): Expr = {
    val Identifier(name, sourceInfoOpt) = identifier
    if (bindings contains name)
      identifier
    else {
      for {
        sourceInfo ← sourceInfoOpt
        token ← sourceInfo.node.tokens
      } bareWordTokens += token
      StringLiteral(name, QuotationType.Double, sourceInfoOpt = sourceInfoOpt)
    }
  }

  private def bareStringify(statementSeq: StatementSeq, bindings: Set[String]): StatementSeq = {
    val StatementSeq(statements, sourceInfoOpt) = statementSeq
    var newStatements = ArrayBuffer[Expr]()
    var latestBindings = bindings
    for (statement ← statements) {
      newStatements += bareStringify(statement, latestBindings)
      latestBindings = latestBindings ++ getNewlyBoundNames(statement)
    }
    StatementSeq(newStatements, sourceInfoOpt)
  }

  private def getNewlyBoundNames(left: Expr): Seq[String] =
    left.findAll {
      case AssignmentExpr(left@Identifier(name, _), _, _, _) ⇒ Seq(name)
      case PatternAssignmentExpr(pattern, _, _)              ⇒ pattern.boundNames
      case FunctionDeclaration(_, _, name, _, _, _)          ⇒ Seq(name)
      case ClassDeclaration(_, _, name, _, _, _)             ⇒ Seq(name)
    }.flatten

}