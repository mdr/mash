package com.github.mdr.mash.inference

import java.nio.file.Path
import java.time.Instant
import java.util.IdentityHashMap
import scala.PartialFunction.condOpt
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.ns.collections.SeqClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.BinaryOperator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.evaluator.SystemCommandFunction
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.parser.QuotationType
import com.github.mdr.mash.ns.os.ProcessResultClass

case class AnnotatedExpr(exprOpt: Option[Expr], typeOpt: Option[Type])

object TypeInferencer {

  def buildBindings(env: Environment, includeGlobal: Boolean = true): Map[String, Type] = {

    val envBindings =
      for ((k, v) ← env.bindings)
        yield k -> ValueTypeDetector.getType(v)
    if (includeGlobal) {
      val globalBindings =
        for ((k, v) ← env.globalVariables.toMap)
          yield k -> ValueTypeDetector.getType(v)
      globalBindings ++ envBindings
    } else
      envBindings
  }

}

class TypeInferencer {

  /**
   * We maintain a visited map to avoid loops in the type inference (this can happen, for example, when
   * typing fixed-point combinators)
   */
  private val visitedMap: IdentityHashMap[Expr, Boolean] = new IdentityHashMap

  /**
   * Attempt to infer the type of a given expression (and subexpression).
   *
   * @param bindings -- known type information about the variable bindings available to this expression.
   * @param immediateExec -- if true, immediately "execute" (at the type level) nullary functions and methods
   */
  def inferType(expr: Expr, bindings: Map[String, Type], immediateExec: Boolean = true): Option[Type] =
    if (visitedMap.containsKey(expr))
      expr.typeOpt
    else {
      visitedMap.put(expr, true)
      try {
        val typeOpt = inferType_(expr, bindings, immediateExec)
        expr.typeOpt = typeOpt
        expr.typeBindings = bindings
        typeOpt
      } finally
        visitedMap.remove(expr)
    }

  private def inferType_(expr: Expr, bindings: Map[String, Type], immediateExec: Boolean = true): Option[Type] =
    expr match {
      case Hole(_) | PipeExpr(_, _, _)                  ⇒ None // should not be present in AST at this point
      case MishFunction(command, _)                     ⇒ Some(Type.DefinedFunction(SystemCommandFunction(command)))
      case ParenExpr(body, _)                           ⇒ inferType(body, bindings)
      case Literal(x, _)                                ⇒ Some(ValueTypeDetector.getType(x))
      case StringLiteral(s, QuotationType.Double, _, _) ⇒ Some(Type.Tagged(StringClass, PathClass))
      case StringLiteral(s, QuotationType.Single, _, _) ⇒ Some(Type.Instance(StringClass))
      case InterpolatedString(_, parts, _, _) ⇒
        parts.foreach {
          case StringPart(s) ⇒
          case ExprPart(expr) ⇒
            inferType(expr, bindings)
        }
        Some(Type.Tagged(StringClass, PathClass))
      case MinusExpr(_, _)      ⇒ Some(Type.Instance(NumberClass))
      case binOpExpr: BinOpExpr ⇒ inferTypeBinOpExpr(binOpExpr, bindings)
      case AssignmentExpr(left, right, _, _) ⇒
        inferType(right, bindings)
        Some(Type.Instance(UnitClass))
      case identifier: Identifier ⇒ inferTypeIdentifier(identifier, bindings, immediateExec)
      case LambdaExpr(v, body, _) ⇒
        inferType(body, bindings + (v -> Type.Any)) // Preliminary -- might be updated to be more precise in an outer context
        Some(Type.Lambda(v, body, bindings))
      case invocationExpr: InvocationExpr ⇒ inferTypeInvocationExpr(invocationExpr, bindings)
      case ListExpr(items, _) ⇒
        val elementTypes = items.flatMap(inferType(_, bindings))
        val elementType = elementTypes.headOption.getOrElse(Type.Any)
        Some(Type.Seq(elementType))
      case memberExpr: MemberExpr ⇒ inferTypeMemberExpr(memberExpr, bindings, immediateExec)
      case lookupExpr: LookupExpr ⇒ inferTypeLookupExpr(lookupExpr, bindings)
      case ifExpr: IfExpr         ⇒ inferTypeIfExpr(ifExpr, bindings)
      case objectExpr: ObjectExpr ⇒ inferTypeObjectExpr(objectExpr, bindings)
      case MishExpr(command, args, captureProcessOutput, _) ⇒
        inferType(command, bindings)
        args.foreach(inferType(_, bindings))
        val resultClass = if (captureProcessOutput) ProcessResultClass else UnitClass
        Some(Type.Instance(resultClass))
      case MishInterpolation(part, _) ⇒
        part match {
          case StringPart(s) ⇒
            Some(Type.Instance(StringClass))
          case ExprPart(expr) ⇒
            inferType(expr, bindings)
        }
      case FunctionDeclaration(name, params, body, _) ⇒
        Some(Type.Instance(UnitClass))
      case HelpExpr(subexpr, _) ⇒
        inferType(subexpr, bindings, immediateExec = false) flatMap {
          case Type.DefinedFunction(_) ⇒
            Some(Type.Instance(FunctionHelpClass))
          case _ ⇒
            None
        }
    }

  private def inferTypeIdentifier(identifier: Identifier, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] = {
    val Identifier(s, _) = identifier
    bindings.get(s).flatMap {
      case Type.DefinedFunction(f) if f.allowsNullary && immediateExec ⇒
        f.typeInferenceStrategy.inferTypes(InferencerImpl(this, bindings), SimpleTypedArguments(Seq()))
      case x ⇒ Some(x)
    }
  }

  private def inferTypeObjectExpr(objectExpr: ObjectExpr, bindings: Map[String, Type]): Option[Type] = {
    val ObjectExpr(entries, _) = objectExpr
    val fieldTypes = for {
      (label, value) ← entries
      typ_ ← inferType(value, bindings)
    } yield label -> typ_
    Some(Type.Object(fieldTypes))
  }

  private def inferTypeMemberExpr(memberExpr: MemberExpr, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] = {
    val MemberExpr(target, name, _, _) = memberExpr
    for {
      targetType ← inferType(target, bindings)
      memberType ← memberLookup(targetType, name, immediateExec = immediateExec)
    } yield memberType
  }

  private def inferTypeBinOpExpr(binOpExpr: Expr, bindings: Map[String, Type]): Option[Type] = {
    import BinaryOperator._
    val BinOpExpr(left, op, right, _) = binOpExpr
    val leftTypeOpt = inferType(left, bindings)
    val rightTypeOpt = inferType(right, bindings)
    op match {
      case Plus ⇒
        (leftTypeOpt, rightTypeOpt) match {
          case (Some(Type.Seq(elementType)), Some(Type.Seq(_))) ⇒ leftTypeOpt
          case (Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass)), _) ⇒ leftTypeOpt
          case (_, Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass))) ⇒ rightTypeOpt
          case (Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass)), _) ⇒ leftTypeOpt
          case (_, Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass))) ⇒ rightTypeOpt
          case _ ⇒ Some(Type.Instance(NumberClass))
        }
      case Multiply ⇒
        (leftTypeOpt, rightTypeOpt) match {
          case (Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass)), Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass))) ⇒ rightTypeOpt
          case (Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass)), Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass))) ⇒ rightTypeOpt
          case (Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass)), Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass))) ⇒ leftTypeOpt
          case _ ⇒ Some(Type.Instance(NumberClass))
        }
      case Minus | Divide ⇒
        (leftTypeOpt, rightTypeOpt) match {
          case (Some(Type.Tagged(NumberClass, _)), _) ⇒ leftTypeOpt
          case (_, Some(Type.Tagged(NumberClass, _))) ⇒ rightTypeOpt
          case _                                      ⇒ Some(Type.Instance(NumberClass))
        }
      case Equals | NotEquals | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals ⇒
        Some(Type.Instance(BooleanClass))
      case Sequence ⇒
        rightTypeOpt
      case And | Or ⇒
        leftTypeOpt orElse rightTypeOpt
    }
  }

  private def annotateArg(arg: Argument, bindings: Map[String, Type]): TypedArgument =
    arg match {
      case Argument.PositionArg(e, _) ⇒
        TypedArgument.PositionArg(AnnotatedExpr(Some(e), inferType(e, bindings)))
      case Argument.ShortFlag(flags, _) ⇒
        TypedArgument.ShortFlag(flags)
      case Argument.LongFlag(flag, valueOpt, _) ⇒
        TypedArgument.LongFlag(flag, valueOpt.map(e ⇒ AnnotatedExpr(Some(e), inferType(e, bindings))))
    }

  private def inferTypeInvocationExpr(invocationExpr: InvocationExpr, bindings: Map[String, Type]): Option[Type] = {
    val InvocationExpr(f, args, _) = invocationExpr
    val typedArgs = SimpleTypedArguments(args.map(annotateArg(_, bindings)))
    inferType(f, bindings, immediateExec = false).flatMap {
      case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒
        for {
          arg ← typedArgs.positionArgs.headOption
          StringLiteral(s, _, _, _) ← Some(f)
          argType ← arg.typeOpt
          memberType ← memberLookup(argType, s, immediateExec = true)
        } yield memberType
      case Type.Seq(Type.BoundMethod(receiverType, method)) ⇒
        val strategy = method.typeInferenceStrategy
        val arguments = SimpleTypedArguments(typedArgs.arguments)
        strategy.inferTypes(InferencerImpl(this, bindings), Some(receiverType), arguments).map(Type.Seq)
      case Type.Seq(elementType) ⇒
        Some(elementType)
      case Type.BoundMethod(receiverType, method) ⇒
        val strategy = method.typeInferenceStrategy
        val arguments = SimpleTypedArguments(typedArgs.arguments)
        strategy.inferTypes(InferencerImpl(this, bindings), Some(receiverType), arguments)
      case Type.DefinedFunction(f) ⇒
        val strategy = f.typeInferenceStrategy
        strategy.inferTypes(InferencerImpl(this, bindings), typedArgs)
      case Type.Lambda(parameter, expr, lambdaBindings) ⇒
        for {
          AnnotatedExpr(_, typeOpt) ← typedArgs.positionArgs.headOption
          argType ← typeOpt
          typ ← inferType(expr, lambdaBindings ++ bindings + (parameter -> argType))
        } yield typ
      case _ ⇒ None
    }
  }

  private def inferTypeIfExpr(ifExpr: IfExpr, bindings: Map[String, Type]): Option[Type] = {
    val IfExpr(cond, body, elseOpt, _) = ifExpr
    inferType(cond, bindings)
    val bodyTypeOpt = inferType(body, bindings)
    val elseTypeOpt = elseOpt.flatMap(inferType(_, bindings))
    bodyTypeOpt // If the elseType is different, tough cheese
  }

  private def memberLookup(typ: Type, klass: MashClass, name: String): Option[Type] =
    klass.fieldsMap.get(name).map(_.fieldType) orElse
      klass.getMethod(name).map { method ⇒ Type.BoundMethod(typ, method) } orElse
      klass.parentOpt.flatMap(superClass ⇒ memberLookup(typ, superClass, name))

  def memberLookup(typ: Type, name: String, immediateExec: Boolean): Option[Type] = {
    val intermediate = typ match {
      case Type.Instance(klass)             ⇒ memberLookup(typ, klass, name)
      case Type.Tagged(baseClass, tagClass) ⇒ memberLookup(typ, baseClass, name) orElse memberLookup(typ, tagClass, name)
      case Type.Seq(elementType)            ⇒ memberLookup(typ, SeqClass, name) orElse memberLookup(elementType, name, immediateExec).map(Type.Seq)
      case Type.Object(knownFields)         ⇒ knownFields.get(name) orElse memberLookup(typ, ObjectClass, name)
      case Type.DefinedFunction(_)          ⇒ memberLookup(typ, FunctionClass, name)
      case Type.BoundMethod(_, _)           ⇒ memberLookup(typ, BoundMethodClass, name)
      case Type.Group(keyType, elementType) ⇒
        if (name == GroupClass.Fields.Key.name)
          Some(keyType)
        else if (name == GroupClass.Fields.Values.name)
          Some(Type.Seq(elementType))
        else
          memberLookup(typ, GroupClass, name)
      case _ ⇒ None
    }
    if (immediateExec)
      intermediate match {
        case Some(Type.BoundMethod(typ, method)) if method.allowsNullary ⇒
          method.typeInferenceStrategy.inferTypes(InferencerImpl(this, Map()), Some(typ), SimpleTypedArguments(Seq()))
        case Some(Type.DefinedFunction(f)) if f.allowsNullary ⇒
          val arguments = SimpleTypedArguments(Seq())
          f.typeInferenceStrategy.inferTypes(InferencerImpl(this, Map()), arguments)
        case x ⇒
          x
      }
    else
      intermediate
  }

  private def inferTypeLookupExpr(lookupExpr: LookupExpr, bindings: Map[String, Type]): Option[Type] = {
    val LookupExpr(targetExpr, indexExpr, _) = lookupExpr
    val targetTypeOpt = inferType(targetExpr, bindings)
    val indexTypeOpt = inferType(indexExpr, bindings)
    indexExpr match {
      case StringLiteral(s, _, _, _) ⇒ return targetTypeOpt.flatMap(memberLookup(_, s, immediateExec = true))
      case _                         ⇒
    }
    condOpt((targetTypeOpt, indexTypeOpt)) {
      case (Some(Type.Seq(elementType)), Some(Type.Instance(NumberClass)))                    ⇒ elementType
      case (Some(Type.Instance(StringClass)), Some(Type.Instance(NumberClass)))               ⇒ Type.Instance(StringClass)
      case (Some(taggedType @ Type.Tagged(StringClass, _)), Some(Type.Instance(NumberClass))) ⇒ taggedType
    }
  }

}
