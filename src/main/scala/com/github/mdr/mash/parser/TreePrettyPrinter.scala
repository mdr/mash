package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.AbstractSyntax._
import scala.PartialFunction._
import java.text.DecimalFormat
import com.github.mdr.mash.utils.NumberUtils
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.evaluator.MashString

object TreePrettyPrinter {

  /**
   * Print an expression as an indented tree
   */
  def printTree(expr: Expr, depth: Int = 0): Unit = {
    val indent = "  " * depth
    print(indent)
    val typeDescription = expr.typeOpt.map(" [" + _ + "]").getOrElse("")
    expr match {
      case Literal(_, _) | StringLiteral(_, _, _, _) | Identifier(_, _) | Hole(_) | MishFunction(_, _) ⇒
        println(PrettyPrinter.pretty(expr) + typeDescription)
      case InterpolatedString(start, parts, end, _) ⇒
        println("InterpolatedString" + typeDescription)
        println("  " * (depth + 1) + start)
        parts.foreach {
          case StringPart(s)  ⇒ println("  " * (depth + 1) + s)
          case ExprPart(expr) ⇒ printTree(expr, depth + 1)
        }
        println("  " * (depth + 1) + end)
      case IfExpr(cond, body, elseOpt, _) ⇒
        println("IfExpr" + typeDescription)
        printTree(cond, depth + 1)
        printTree(body, depth + 1)
        for (elseBody ← elseOpt)
          printTree(elseBody, depth + 1)
      case MemberExpr(left, member, isNullSafe, _) ⇒
        println("MemberExpr" + typeDescription + (if (isNullSafe) "(null safe)" else ""))
        printTree(left, depth + 1)
        println("  " * (depth + 1) + member)
      case MinusExpr(subExpr, _) ⇒
        println("MinusExpr" + typeDescription)
        printTree(subExpr, depth + 1)
      case LookupExpr(expr, index, _) ⇒
        println("LookupExpr" + typeDescription)
        printTree(expr, depth + 1)
        printTree(index, depth + 2)
      case InvocationExpr(function, args, _) ⇒
        println("InvocationExpr" + typeDescription)
        printTree(function, depth + 1)
        for (arg ← args)
          arg match {
            case Argument.PositionArg(e, _) ⇒
              printTree(e, depth + 1)
            case Argument.ShortFlag(flags, _) ⇒
              println("  " * (depth + 1) + "-" + flags)
            case Argument.LongFlag(flag, valueOpt, _) ⇒
              println("  " * (depth + 1) + "--" + flag)
              for (value ← valueOpt)
                printTree(value, depth + 2)
          }
      case PipeExpr(left, right, _) ⇒
        println("PipeExpr" + typeDescription)
        printTree(left, depth + 1)
        printTree(right, depth + 1)
      case ParenExpr(body, _) ⇒
        println("ParenExpr" + typeDescription)
        printTree(body, depth + 1)
      case LambdaExpr(v, body, _) ⇒
        println("LambdaExpr" + typeDescription)
        println("  " * (depth + 1) + v)
        printTree(body, depth + 1)
      case BinOpExpr(left, op, right, _) ⇒
        println("BinOpExpr: " + op + typeDescription)
        printTree(left, depth + 1)
        printTree(right, depth + 1)
      case AssignmentExpr(left, right, _) ⇒
        println("AssignmentExpr" + typeDescription)
        printTree(left, depth + 1)
        printTree(right, depth + 1)
      case ListExpr(items, _) ⇒
        println("ListExpr" + typeDescription)
        for (item ← items)
          printTree(item, depth + 1)
      case ObjectExpr(entries, _) ⇒
        println("ObjectExpr" + typeDescription)
        for ((label, body) ← entries) {
          println("  " * (depth + 1) + label)
          printTree(body, depth + 2)
        }
      case MishInterpolation(part, _) ⇒
        println("MishInterpolation" + typeDescription)
        part match {
          case StringPart(s)  ⇒ println("  " * (depth + 1) + s)
          case ExprPart(expr) ⇒ printTree(expr, depth + 1)
        }
      case MishExpr(command, args, _) ⇒
        println("MishExpr" + typeDescription)
        printTree(command, depth + 1)
        for (arg ← args)
          printTree(arg, depth + 1)
      case FunctionDeclaration(name, params, body, _) ⇒
        println("FunctionDeclaration" + typeDescription)
        println("  " * (depth + 1) + name)
        for (param ← params)
          println("  " * (depth + 1) + param)
        printTree(body, depth + 1)
      case HelpExpr(subExpr, _) ⇒
        println("HelpExpr" + typeDescription)
        printTree(subExpr, depth + 1)

    }

  }

}