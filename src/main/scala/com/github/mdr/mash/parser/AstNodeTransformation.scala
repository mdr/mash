package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.AbstractSyntax.{ ListPattern, _ }

trait AstNodeTransformation {
  self: AbstractSyntax.AstNode ⇒

  def transform(f: PartialFunction[AstNode, AstNode]): AstNode = {
    val withTransformedDescendents = transformDescendents(f)
    f.lift.apply(withTransformedDescendents).getOrElse(withTransformedDescendents)
  }

  private def transformDescendents(f: PartialFunction[AstNode, AstNode]) = this match {
    case Hole(_) | Literal(_, _) | StringLiteral(_, _, _, _) | Identifier(_, _) | MishFunction(_, _) |
         HeadlessMemberExpr(_, _, _) | _: ShorthandObjectEntry | ThisExpr(_) | NamespaceDeclaration(_, _)
                                                                                            ⇒
      this
    case InterpolatedString(start, parts, end, sourceInfoOpt)                               ⇒
      InterpolatedString(start, parts.map {
        case StringPart(s) ⇒ StringPart(s)
        case ExprPart(e)   ⇒ ExprPart(e.transform(f))
      }, end, sourceInfoOpt)
    case AssignmentExpr(left, operatorOpt, right, sourceInfoOpt)                            ⇒
      AssignmentExpr(left.transform(f), operatorOpt, right.transform(f), sourceInfoOpt)
    case PatternAssignmentExpr(pattern, right, sourceInfoOpt)                               ⇒
      PatternAssignmentExpr(pattern.transform(f).asInstanceOf[Pattern], right.transform(f), sourceInfoOpt)
    case ParenExpr(expr, sourceInfoOpt)                                                     ⇒
      ParenExpr(expr.transform(f), sourceInfoOpt)
    case BlockExpr(expr, sourceInfoOpt)                                                     ⇒
      BlockExpr(expr.transform(f), sourceInfoOpt)
    case StatementSeq(statements, sourceInfoOpt)                                            ⇒
      StatementSeq(statements.map(_.transform(f)), sourceInfoOpt)
    case LambdaExpr(params, body, sourceInfoOpt)                                            ⇒
      LambdaExpr(params.transform(f).asInstanceOf[ParamList], body.transform(f), sourceInfoOpt)
    case PipeExpr(left, right, sourceInfoOpt)                                               ⇒
      PipeExpr(left.transform(f), right.transform(f), sourceInfoOpt)
    case MemberExpr(expr, name, isNullSafe, sourceInfoOpt)                                  ⇒
      MemberExpr(expr.transform(f), name, isNullSafe, sourceInfoOpt)
    case LookupExpr(expr, index, sourceInfoOpt)                                             ⇒
      LookupExpr(expr.transform(f), index.transform(f), sourceInfoOpt)
    case InvocationExpr(function, arguments, isParenInvocation, sourceInfoOpt)              ⇒
      val newArguments = arguments.map(_.transform(f).asInstanceOf[Argument])
      InvocationExpr(function.transform(f), newArguments, isParenInvocation, sourceInfoOpt)
    case BinOpExpr(left, op, right, sourceInfoOpt)                                          ⇒
      BinOpExpr(left.transform(f), op, right.transform(f), sourceInfoOpt)
    case ChainedOpExpr(left, opRights, sourceInfoOpt)                                       ⇒
      ChainedOpExpr(left.transform(f), opRights.map { case (op, right) ⇒ op -> right.transform(f) }, sourceInfoOpt)
    case IfExpr(cond, body, elseOpt, sourceInfoOpt)                                         ⇒
      IfExpr(cond.transform(f), body.transform(f), elseOpt.map(_.transform(f)), sourceInfoOpt)
    case ListExpr(elements, sourceInfoOpt)                                                  ⇒
      ListExpr(elements.map(_.transform(f)), sourceInfoOpt)
    case FullObjectEntry(field, value, sourceInfoOpt)                                       ⇒
      FullObjectEntry(field.transform(f), value.transform(f), sourceInfoOpt)
    case ObjectExpr(entries, sourceInfoOpt)                                                 ⇒
      ObjectExpr(entries.map(_.transform(f).asInstanceOf[ObjectEntry]), sourceInfoOpt)
    case MinusExpr(expr, sourceInfoOpt)                                                     ⇒
      MinusExpr(expr.transform(f), sourceInfoOpt)
    case MishInterpolation(part, sourceInfoOpt)                                             ⇒
      val newPart = part match {
        case StringPart(s) ⇒ StringPart(s)
        case ExprPart(e)   ⇒ ExprPart(e.transform(f))
      }
      MishInterpolation(newPart, sourceInfoOpt)
    case MishExpr(command, args, redirects, captureProcessOutput, sourceInfoOpt)            ⇒
      MishExpr(command.transform(f), args.map(_.transform(f)),
        redirects.map(_.transform(f).asInstanceOf[MishRedirect]), captureProcessOutput, sourceInfoOpt)
    case MishRedirect(op, arg, sourceInfoOpt)                                               ⇒
      MishRedirect(op, arg.transform(f), sourceInfoOpt)
    case FunctionDeclaration(docCommentOpt, attributes, name, params, body, sourceInfoOpt)  ⇒
      val transformedAttributes = attributes.map(_.transform(f).asInstanceOf[Attribute])
      val transformedParams = params.transform(f).asInstanceOf[ParamList]
      FunctionDeclaration(docCommentOpt, transformedAttributes, name, transformedParams, body.transform(f), sourceInfoOpt)
    case ClassDeclaration(docCommentOpt, attributes, name, params, bodyOpt, sourceInfoOpt)  ⇒
      val transformedAttributes = attributes.map(_.transform(f).asInstanceOf[Attribute])
      val transformedParams = params.transform(f).asInstanceOf[ParamList]
      val transformedBodyOpt = bodyOpt.map(_.transform(f).asInstanceOf[ClassBody])
      ClassDeclaration(docCommentOpt, transformedAttributes, name, transformedParams, transformedBodyOpt, sourceInfoOpt)
    case ClassBody(methods, sourceInfoOpt)                                                  ⇒
      ClassBody(methods.map(_.transform(f).asInstanceOf[FunctionDeclaration]), sourceInfoOpt)
    case HelpExpr(expr, sourceInfoOpt)                                                      ⇒
      HelpExpr(expr.transform(f), sourceInfoOpt)
    case ExprPart(expr)                                                                     ⇒
      ExprPart(expr.transform(f))
    case StringPart(_)                                                                      ⇒
      this
    case FunctionParam(attributes, name, isVariadic, defaultOpt, patternOpt, sourceInfoOpt) ⇒
      val transformedAttributes = attributes.map(_.transform(f).asInstanceOf[Attribute])
      val transformedPattern = patternOpt.map(_.transform(f).asInstanceOf[Pattern])
      FunctionParam(transformedAttributes, name, isVariadic, defaultOpt.map(_.transform(f)), transformedPattern, sourceInfoOpt)
    case Argument.PositionArg(expr, sourceInfoOpt)                                          ⇒
      Argument.PositionArg(expr.transform(f), sourceInfoOpt)
    case Argument.ShortFlag(_, _)                                                           ⇒
      this
    case Argument.LongFlag(flag, valueOpt, sourceInfoOpt)                                   ⇒
      Argument.LongFlag(flag, valueOpt.map(_.transform(f)), sourceInfoOpt)
    case ParamList(params)                                                                  ⇒
      ParamList(params.map(_.transform(f).asInstanceOf[FunctionParam]))
    case HolePattern(_) | IdentPattern(_, _)                                                ⇒
      this
    case ObjectPatternEntry(field, valuePatternOpt, sourceInfoOpt)                          ⇒
      ObjectPatternEntry(field, valuePatternOpt.map(_.transform(f).asInstanceOf[Pattern]), sourceInfoOpt)
    case ObjectPattern(entries, sourceInfoOpt)                                              ⇒
      ObjectPattern(entries.map(_.transform(f).asInstanceOf[ObjectPatternEntry]), sourceInfoOpt)
    case ListPattern(patterns, sourceInfoOpt)                                               ⇒
      ListPattern(patterns.map(_.transform(f).asInstanceOf[Pattern]), sourceInfoOpt)
    case Program(namespaceOpt, body, sourceInfoOpt)                                         ⇒
      Program(namespaceOpt, body.transform(f), sourceInfoOpt)
    case Attribute(name, argumentsOpt, sourceInfoOpt)                                       ⇒
      Attribute(name, argumentsOpt.map(_.map(_.transform(f).asInstanceOf[Argument])), sourceInfoOpt)
  }

}
