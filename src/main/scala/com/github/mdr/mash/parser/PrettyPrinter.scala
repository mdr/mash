package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.utils.NumberUtils

import scala.PartialFunction._

/**
  * Routines to print out syntax trees for debugging purposes.
  */
object PrettyPrinter {

  /**
    * Pretty-print the expression
    */
  def pretty(node: AstNode): String = node match {
    case IfExpr(cond, body, elseOpt, sourceInfoOpt)                            ⇒ "if " + pretty(cond) + " then " + pretty(body) + elseOpt.map(elseBody ⇒ " " + pretty(elseBody)).getOrElse("")
    case StringLiteral(s, _, _, _)                                             ⇒ '"' + s + '"' // TOOD: more faithful representation
    case Literal(n: MashNumber, _)                                             ⇒ NumberUtils.prettyString(n.n)
    case Literal(x, _)                                                         ⇒ x + ""
    case InterpolatedString(start, parts, end, _)                              ⇒
      val chunks = parts.map {
        case StringPart(s)  ⇒ s
        case ExprPart(expr) ⇒ pretty(expr)
      }
      "\"" + start + chunks.mkString + end + "\""
    case Identifier(n, _)                                                      ⇒ n.toString
    case MishFunction(w, _)                                                    ⇒ "!" + w.toString
    case Hole(_)                                                               ⇒ "_"
    case MinusExpr(subExpr, _)                                                 ⇒ "-" + parens(pretty(subExpr))
    case MemberExpr(left, member, isSafe, _)                                   ⇒ parens(pretty(left), simpleOmitParens(left)) + (if (isSafe) "?." else ".") + member
    case HeadlessMemberExpr(member, isSafe, _)                                 ⇒ (if (isSafe) "?." else ".") + member
    case LookupExpr(expr, index, _)                                            ⇒ parens(pretty(expr), simpleOmitParens(expr)) + "[" + pretty(index) + "]"
    case PipeExpr(left, right, _)                                              ⇒ "(" + pretty(left) + ") | (" + pretty(right) + ")"
    case ParenExpr(body, _)                                                    ⇒ "(" + pretty(body) + ")"
    case BlockExpr(body, _)                                                    ⇒ "{" + pretty(body) + "}"
    case StatementSeq(statements, _)                                           ⇒ statements.map(pretty).mkString("{ ", "; ", " }")
    case LambdaExpr(v, body, _)                                                ⇒ v.params.map(_.nameOpt.getOrElse("_")).mkString(" ") + " => " + pretty(body)
    case BinOpExpr(left, op, right, _)                                         ⇒ parens(pretty(left), simpleOmitParens(left)) + " " + pretty(op) + " " + parens(pretty(right), simpleOmitParens(right))
    case ListExpr(elements, _)                                                 ⇒ elements.mkString("[", ", ", "]")
    case MishExpr(command, args, redirects, _, _)                              ⇒ pretty(command) + " " + args.map(pretty)
    case ObjectExpr(entries, _)                                                ⇒
      entries.map {
        case FullObjectEntry(field, value, _) ⇒ s"${pretty(field)}: ${pretty(value)}"
        case ShorthandObjectEntry(field, _)   ⇒ field
      }.mkString("{ ", ", ", " }")
    case AssignmentExpr(left, operatorOpt, right, _)                           ⇒
      val operatorSymbol = operatorOpt match {
        case Some(BinaryOperator.Plus)     ⇒ "+="
        case Some(BinaryOperator.Minus)    ⇒ "-="
        case Some(BinaryOperator.Multiply) ⇒ "*="
        case Some(BinaryOperator.Divide)   ⇒ "/="
        case _                             ⇒ "="
      }
      parens(pretty(left), simpleOmitParens(left)) + " " + operatorSymbol + " " + parens(pretty(right), simpleOmitParens(right))
    case PatternAssignmentExpr(pattern, right, _)                              ⇒
      s"$pattern = ${parens(pretty(right), simpleOmitParens(right))}"
    case ObjectPattern(fieldNames, _)                                          ⇒
      fieldNames.mkString("{ ", ", ", " }")
    case ChainedOpExpr(left, opRights, _)                                      ⇒
      parens(pretty(left), simpleOmitParens(left)) +
        opRights.map {
          case (op, right) ⇒
            " " + pretty(op) + " " + parens(pretty(right), simpleOmitParens(right))
        }.mkString
    case InvocationExpr(function, args, _, _)                                  ⇒
      parens(pretty(function), simpleOmitParens(function)) + " " + args.map {
        case Argument.PositionArg(e, _)           ⇒
          parens(pretty(e), simpleOmitParens(e))
        case Argument.ShortFlag(flags, _)         ⇒
          "-" + flags
        case Argument.LongFlag(flag, valueOpt, _) ⇒
          "--" + flag + valueOpt.map(value ⇒ "=" + parens(pretty(value), simpleOmitParens(value))).getOrElse("")
      }.mkString(" ")
    case MishInterpolation(part, _)                                            ⇒
      part match {
        case StringPart(s)  ⇒ s
        case ExprPart(expr) ⇒ pretty(expr)
      }
    case FunctionDeclaration(docCommentOpt, attributes, name, params, body, _) ⇒
      val attributeString = attributes.map("@" + _.name + " ").mkString
      val preparams = if (params.params.isEmpty) "" else " "
      attributeString + "def " + name + preparams + params.params.map(pretty).mkString(" ") + " = " + pretty(body)
    case FunctionParam(attributes, nameOpt, isVariadic, defaultExprOpt, _, _)  ⇒
      val attributeString = attributes.map("@" + _.name + " ").mkString
      var descr = attributeString
      descr += nameOpt.getOrElse("_")
      if (isVariadic)
        descr += "..."
      for (defaultExpr ← defaultExprOpt)
        descr += " = " + pretty(defaultExpr)
      if (defaultExprOpt.isDefined || attributeString.nonEmpty)
        descr = s"($descr)"
      descr
    case HelpExpr(subExpr, _)                                                  ⇒ pretty(subExpr) + "?"
    case ImportStatement(expr, importNameOpt, _)                               ⇒ "import " + pretty(expr) + "." + importNameOpt.getOrElse("_")
    case _                                                                     ⇒ ""
  }

  private def pretty(op: BinaryOperator): String = op match {
    case BinaryOperator.And               ⇒ "and"
    case BinaryOperator.Or                ⇒ "or"
    case BinaryOperator.Equals            ⇒ "=="
    case BinaryOperator.NotEquals         ⇒ "!="
    case BinaryOperator.GreaterThan       ⇒ ">"
    case BinaryOperator.GreaterThanEquals ⇒ ">="
    case BinaryOperator.LessThan          ⇒ "<"
    case BinaryOperator.LessThanEquals    ⇒ "<="
    case BinaryOperator.Plus              ⇒ "+"
    case BinaryOperator.Minus             ⇒ "-"
    case BinaryOperator.Multiply          ⇒ "*"
    case BinaryOperator.Divide            ⇒ "/"
    case BinaryOperator.Sequence          ⇒ ";"
  }

  private def parens(s: String, omitParens: Boolean = false): String = if (omitParens) s else "(" + s + ")"

  private def simpleOmitParens(expr: Expr): Boolean = cond(expr) {
    case Identifier(_, _) | Literal(_, _) | Hole(_) | MemberExpr(_, _, _, _) | ParenExpr(_, _) ⇒ true
  }

}