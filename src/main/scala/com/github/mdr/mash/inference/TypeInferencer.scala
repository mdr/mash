package com.github.mdr.mash.inference

import java.nio.file.Path
import java.time.Instant
import java.util.IdentityHashMap
import scala.PartialFunction.condOpt
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.BinaryOperator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.evaluator.SystemCommandFunction
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.parser.QuotationType
import com.github.mdr.mash.ns.os.ProcessResultClass
import com.github.mdr.mash.runtime.MashValue
import scala.collection.immutable.ListMap

case class AnnotatedExpr(exprOpt: Option[Expr], typeOpt: Option[Type])

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
      case memberExpr: HeadlessMemberExpr               ⇒ None // should not be present in AST at this point
      case MishFunction(command, _)                     ⇒ Some(Type.DefinedFunction(SystemCommandFunction(command)))
      case ParenExpr(body, _)                           ⇒ inferType(body, bindings)
      case Literal(x, _)                                ⇒ Some(ValueTypeDetector.getType(x))
      case StringLiteral(s, QuotationType.Double, _, _) ⇒ Some(StringClass taggedWith PathClass)
      case StringLiteral(s, QuotationType.Single, _, _) ⇒ Some(Type.Instance(StringClass))
      case is: InterpolatedString                       ⇒ inferType(is, bindings)
      case MinusExpr(_, _)                              ⇒ Some(Type.Instance(NumberClass))
      case binOpExpr: BinOpExpr                         ⇒ inferTypeBinOpExpr(binOpExpr, bindings)
      case chainedOpExpr: ChainedOpExpr                 ⇒ inferTypeChainedOpExpr(chainedOpExpr, bindings)
      case memberExpr: MemberExpr                       ⇒ inferType(memberExpr, bindings, immediateExec)
      case lookupExpr: LookupExpr                       ⇒ inferType(lookupExpr, bindings)
      case ifExpr: IfExpr                               ⇒ inferType(ifExpr, bindings)
      case objectExpr: ObjectExpr                       ⇒ inferType(objectExpr, bindings)
      case FunctionDeclaration(name, params, body, _)   ⇒ Some(Type.Instance(UnitClass))
      case identifier: Identifier                       ⇒ inferType(identifier, bindings, immediateExec)
      case mishExpr: MishExpr                           ⇒ inferType(mishExpr, bindings)
      case invocationExpr: InvocationExpr               ⇒ inferTypeInvocationExpr(invocationExpr, bindings)
      case interpolation: MishInterpolation             ⇒ inferType(interpolation, bindings)
      case listExpr: ListExpr                           ⇒ inferType(listExpr, bindings)
      case StatementSeq(statements, _) ⇒
        statements.flatMap(s ⇒ inferType(s, bindings)).lastOption.orElse(Some(Type.Instance(UnitClass)))
      case AssignmentExpr(left, right, _, _) ⇒
        inferType(left, bindings)
        inferType(right, bindings)
        Some(Type.Instance(UnitClass))
      case LambdaExpr(v, body, _) ⇒
        inferType(body, bindings + (v -> Type.Any)) // Preliminary -- might be updated to be more precise in an outer context
        Some(Type.Lambda(v, body, bindings))
      case HelpExpr(subexpr, _) ⇒
        inferType(subexpr, bindings, immediateExec = false) collect {
          case Type.DefinedFunction(_) ⇒ Type.Instance(FunctionHelpClass)
        }
    }

  private def inferType(listExpr: ListExpr, bindings: Map[String, Type]): Option[Type] = {
    val elementTypes = listExpr.items.flatMap(inferType(_, bindings))
    val elementType = elementTypes.headOption.getOrElse(Type.Any)
    Some(Type.Seq(elementType))
  }

  private def inferType(interpolation: MishInterpolation, bindings: Map[String, Type]): Option[Type] =
    interpolation.part match {
      case StringPart(s)  ⇒ Some(Type.Instance(StringClass))
      case ExprPart(expr) ⇒ inferType(expr, bindings)
    }

  private def inferType(mishExpr: MishExpr, bindings: Map[String, Type]): Option[Type] = {
    val MishExpr(command, args, captureProcessOutput, _) = mishExpr
    inferType(command, bindings)
    args.foreach(inferType(_, bindings))
    val resultClass = if (captureProcessOutput) ProcessResultClass else UnitClass
    Some(Type.Instance(resultClass))
  }

  private def inferType(interpolatedString: InterpolatedString, bindings: Map[String, Type]): Option[Type] = {
    val InterpolatedString(_, parts, _, _) = interpolatedString
    parts.foreach {
      case StringPart(s) ⇒
      case ExprPart(expr) ⇒
        inferType(expr, bindings)
    }
    Some(StringClass taggedWith PathClass)
  }

  private def inferType(identifier: Identifier, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] = {
    val Identifier(s, _) = identifier
    bindings.get(s).flatMap {
      case Type.DefinedFunction(f) if f.allowsNullary && immediateExec ⇒
        f.typeInferenceStrategy.inferTypes(InferencerImpl(this, bindings), SimpleTypedArguments(Seq()))
      case x ⇒ Some(x)
    }
  }

  private def inferType(objectExpr: ObjectExpr, bindings: Map[String, Type]): Option[Type] = {
    val ObjectExpr(entries, _) = objectExpr
    val fieldTypes =
      for {
        (label, value) ← entries
        typ_ ← inferType(value, bindings)
      } yield label -> typ_
    Some(Type.Object(fieldTypes))
  }

  private def inferType(memberExpr: MemberExpr, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] = {
    val MemberExpr(target, name, _, _) = memberExpr
    for {
      targetType ← inferType(target, bindings)
      memberType ← memberLookup(targetType, name, immediateExec = immediateExec)
    } yield memberType
  }

  private def inferTypeChainedOpExpr(chainedOpExpr: Expr, bindings: Map[String, Type]): Option[Type] = {
    val ChainedOpExpr(left, opRights, _) = chainedOpExpr
    inferType(left, bindings)
    for ((op, right) ← opRights) yield op -> inferType(right, bindings)
    Some(Type.Instance(BooleanClass))
  }

  private def inferTypeBinOpExpr(binOpExpr: Expr, bindings: Map[String, Type]): Option[Type] = {
    val BinOpExpr(left, op, right, _) = binOpExpr
    val leftTypeOpt = inferType(left, bindings)
    val rightTypeOpt = inferType(right, bindings)
    inferTypeBinOpExpr(leftTypeOpt, op, rightTypeOpt)
  }

  private def inferTypeAdd(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Seq(elementType)), Some(Type.Seq(_)))                    ⇒ leftTypeOpt
      case (Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass)), _) ⇒ leftTypeOpt
      case (_, Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass))) ⇒ rightTypeOpt
      case (Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass)), _) ⇒ leftTypeOpt
      case (_, Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass))) ⇒ rightTypeOpt
      case _ ⇒
        val objectAdditionTypeOpt =
          for {
            leftFields ← leftTypeOpt.flatMap(fields)
            rightFields ← rightTypeOpt.flatMap(fields)
          } yield Type.Object(leftFields ++ rightFields)
        objectAdditionTypeOpt orElse Some(Type.Instance(NumberClass))
    }

  private def fields(type_ : Type): Option[Map[String, Type]] = condOpt(type_) {
    case Type.Instance(klass) ⇒ klass.fieldsMap.map { case (fieldName, field) ⇒ fieldName -> field.fieldType }
    case Type.Object(fields)  ⇒ fields
    case Type.Group(_, _)     ⇒ GroupClass.fieldsMap.map { case (fieldName, field) ⇒ fieldName -> field.fieldType }
  }

  private def inferTypeMultiply(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass)), Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass))) ⇒ leftTypeOpt
      case (Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass)), Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass) | Type.Seq(_))) ⇒ rightTypeOpt
      case (Some(Type.Tagged(StringClass, _) | Type.Instance(StringClass) | Type.Seq(_)), Some(Type.Tagged(NumberClass, _) | Type.Instance(NumberClass))) ⇒ leftTypeOpt
      case _ ⇒ Some(Type.Instance(NumberClass))
    }

  private def inferTypeBinOpExpr(leftTypeOpt: Option[Type], op: BinaryOperator, rightTypeOpt: Option[Type]): Option[Type] = {
    import BinaryOperator._
    op match {
      case Plus     ⇒ inferTypeAdd(leftTypeOpt, rightTypeOpt)
      case Multiply ⇒ inferTypeMultiply(leftTypeOpt, rightTypeOpt)
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

  private def inferTypeArg(arg: Argument, bindings: Map[String, Type]) {
    arg match {
      case Argument.PositionArg(e, _)              ⇒ inferType(e, bindings)
      case Argument.LongFlag(flag, Some(value), _) ⇒ inferType(value, bindings)
      case _                                       ⇒
    }
  }

  private def inferTypeInvocationExpr(invocationExpr: InvocationExpr, bindings: Map[String, Type]): Option[Type] = {
    val InvocationExpr(f, args, _, _) = invocationExpr

    args.foreach(inferTypeArg(_, bindings))
    val typedArgs = SimpleTypedArguments.from(invocationExpr)

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
      case _ ⇒
        None
    }
  }

  private def inferType(ifExpr: IfExpr, bindings: Map[String, Type]): Option[Type] = {
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
      case Type.Seq(elementType)            ⇒ memberLookup(typ, ListClass, name) orElse memberLookup(elementType, name, immediateExec).map(Type.Seq)
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

  private def inferType(lookupExpr: LookupExpr, bindings: Map[String, Type]): Option[Type] = {
    val LookupExpr(targetExpr, indexExpr, _) = lookupExpr
    val targetTypeOpt = inferType(targetExpr, bindings)
    val indexTypeOpt = inferType(indexExpr, bindings)
    indexExpr match {
      case StringLiteral(s, _, _, _) ⇒ return targetTypeOpt.flatMap(memberLookup(_, s, immediateExec = false))
      case _                         ⇒
    }
    condOpt((targetTypeOpt, indexTypeOpt)) {
      case (Some(Type.Seq(elementType)), Some(Type.Instance(NumberClass)))                    ⇒ elementType
      case (Some(Type.Instance(StringClass)), Some(Type.Instance(NumberClass)))               ⇒ Type.Instance(StringClass)
      case (Some(taggedType @ Type.Tagged(StringClass, _)), Some(Type.Instance(NumberClass))) ⇒ taggedType
    }
  }

}
