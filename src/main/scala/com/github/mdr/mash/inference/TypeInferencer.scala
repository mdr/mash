package com.github.mdr.mash.inference

import java.util.IdentityHashMap

import com.github.mdr.mash.evaluator.{ SystemCommandFunction, _ }
import com.github.mdr.mash.ns.collections.{ GroupClass, ListClass }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.os.{ PathClass, ProcessResultClass }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ BinaryOperator, QuotationType }

import scala.PartialFunction.condOpt

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
      case BlockExpr(body, _)                           ⇒ inferType(body, bindings)
      case Literal(x, _)                                ⇒ Some(ValueTypeDetector.getType(x))
      case StringLiteral(s, QuotationType.Double, _, _) ⇒ Some(StringClass taggedWith PathClass)
      case StringLiteral(s, QuotationType.Single, _, _) ⇒ Some(Type.Instance(StringClass))
      case is: InterpolatedString                       ⇒ inferType(is, bindings)
      case MinusExpr(_, _)                              ⇒ Some(NumberClass)
      case binOpExpr: BinOpExpr                         ⇒ inferTypeBinOpExpr(binOpExpr, bindings)
      case chainedOpExpr: ChainedOpExpr                 ⇒ inferTypeChainedOpExpr(chainedOpExpr, bindings)
      case memberExpr: MemberExpr                       ⇒ inferType(memberExpr, bindings, immediateExec)
      case lookupExpr: LookupExpr                       ⇒ inferType(lookupExpr, bindings)
      case ifExpr: IfExpr                               ⇒ inferType(ifExpr, bindings)
      case objectExpr: ObjectExpr                       ⇒ inferType(objectExpr, bindings)
      case identifier: Identifier                       ⇒ inferType(identifier, bindings, immediateExec)
      case mishExpr: MishExpr                           ⇒ inferType(mishExpr, bindings)
      case invocationExpr: InvocationExpr               ⇒ inferTypeInvocationExpr(invocationExpr, bindings)
      case interpolation: MishInterpolation             ⇒ inferType(interpolation, bindings)
      case listExpr: ListExpr                           ⇒ inferType(listExpr, bindings)
      case assignmentExpr: AssignmentExpr               ⇒ inferType(assignmentExpr, bindings)
      case assignmentExpr: PatternAssignmentExpr        ⇒ inferType(assignmentExpr, bindings)
      case statementSeq: StatementSeq                   ⇒ inferType(statementSeq, bindings)
      case FunctionDeclaration(name, params, body, _)   ⇒
        inferType(body, bindings ++ getPreliminaryBindings(params))
        Some(Unit)
      case LambdaExpr(params, body, _) ⇒
        inferType(body, bindings ++ getPreliminaryBindings(params))
        Some(Type.Function(Evaluator.parameterModel(params), body, bindings))
      case HelpExpr(subExpr, _) ⇒
        inferType(subExpr, bindings, immediateExec = false) collect {
          case Type.DefinedFunction(_) ⇒ FunctionHelpClass
        }
    }

  private def inferType(statementSeq: StatementSeq, bindings: Map[String, Type]): Option[Type] = {
    var latestBindings = bindings
    for (statement <- statementSeq.statements) {
      inferType(statement, latestBindings)
      statement match {
        case AssignmentExpr(Identifier(name, _), _, _, _, _) =>
          statement.typeOpt.foreach(latestBindings += name -> _)
        case PatternAssignmentExpr(pattern, right, _)        =>
          val wholeType = right.typeOpt.getOrElse(Type.Any)
          pattern match {
            case ObjectPattern(fieldNames, _) =>
              val fieldTypes: Map[String, Type] =
                wholeType match {
                  case Type.Object(knownFields) => knownFields
                  case Type.Instance(klass)     => klass.fieldsMap.mapValues(_.fieldType)
                  case _                        => Map()
                }
              for (fieldName <- fieldNames)
                latestBindings += fieldName -> fieldTypes.getOrElse(fieldName, Type.Any)
          }
        case decl@FunctionDeclaration(name, paramList, body, _) =>
          latestBindings += name -> Type.Function(Evaluator.parameterModel(paramList), body, latestBindings)
        case _ =>
      }
    }
    statementSeq.statements.lastOption.map(_.typeOpt).getOrElse(Some(Unit))
  }

  // Preliminary typing pass -- might be updated to be more precise once we know argument types
  private def getPreliminaryBindings(params: ParamList): Map[String, Type] =
    params.params.flatMap(getParamPreliminaryBindings).toMap

  private def getParamPreliminaryBindings(param: FunctionParam): Map[String, Type] =
    (param match {
      case FunctionParam(Some(name), _, _, _, _, _)       ⇒
        Seq(name -> Type.Any)
      case FunctionParam(None, _, _, _, Some(pattern), _) ⇒
        for (name <- pattern.boundNames)
          yield name -> Type.Any
      case _                                              =>
        Seq()
    }).toMap

  private def inferType(assignmentExpr: AssignmentExpr, bindings: Map[String, Type]): Option[Type] = {
    val AssignmentExpr(left, operatorOpt, right, _, _) = assignmentExpr
    val leftTypeOpt = inferType(left, bindings)
    val rightTypeOpt = inferType(right, bindings)
    operatorOpt.flatMap(op ⇒ inferTypeBinOpExpr(leftTypeOpt, op, rightTypeOpt, right)) orElse rightTypeOpt
  }

  private def inferType(assignmentExpr: PatternAssignmentExpr, bindings: Map[String, Type]): Option[Type] = {
    val PatternAssignmentExpr(_, right, _) = assignmentExpr
    val rightTypeOpt = inferType(right, bindings)
    rightTypeOpt
  }

  private def inferType(listExpr: ListExpr, bindings: Map[String, Type]): Option[Type] = {
    val elementTypes = listExpr.items.flatMap(inferType(_, bindings))
    val elementType = elementTypes.headOption.getOrElse(Type.Any)
    Some(elementType.seq)
  }

  private def inferType(interpolation: MishInterpolation, bindings: Map[String, Type]): Option[Type] =
    interpolation.part match {
      case StringPart(s)  ⇒ Some(StringClass)
      case ExprPart(expr) ⇒ inferType(expr, bindings)
    }

  private def inferType(mishExpr: MishExpr, bindings: Map[String, Type]): Option[Type] = {
    val MishExpr(command, args, redirects, captureProcessOutput, _) = mishExpr
    inferType(command, bindings)
    redirects.foreach(redirect ⇒ inferType(redirect.arg, bindings))
    args.foreach(inferType(_, bindings))
    Some(if (captureProcessOutput) ProcessResultClass else UnitClass)
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
      case typ @ Type.DefinedFunction(f) if f.allowsNullary && immediateExec                                          ⇒
        identifier.preInvocationTypeOpt = Some(typ)
        f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), SimpleTypedArguments(Seq()))
      case typ @ Type.Function(parameterModel, body, lambdaBindings) if parameterModel.allowsNullary && immediateExec ⇒
        identifier.preInvocationTypeOpt = Some(typ)
        inferType(body, lambdaBindings)
      case x                                                                                                          ⇒
        Some(x)
    }
  }

  private def inferType(name: String, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] =
    bindings.get(name).flatMap {
      case typ @ Type.DefinedFunction(f) if f.allowsNullary && immediateExec ⇒
        f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), SimpleTypedArguments(Seq()))
      case x ⇒
        Some(x)
    }

  private def inferType(objectExpr: ObjectExpr, bindings: Map[String, Type]): Option[Type] = {
    val ObjectExpr(entries, _) = objectExpr
    val fieldTypes =
      entries.flatMap {
        case FullObjectEntry(fieldExpr, valueExpr, _) =>
          inferType(fieldExpr, bindings)
          for {
            label ← getFieldName(fieldExpr)
            typ_ ← inferType(valueExpr, bindings)
          } yield label -> typ_
        case ShorthandObjectEntry(field, _)           =>
          inferType(field, bindings, immediateExec = true) match {
            case Some(fieldType) => Seq(field -> fieldType)
            case None => Seq(field -> Type.Any)
          }
      }
    Some(Type.Object(fieldTypes.toMap))
  }

  private def getFieldName(label: Expr): Option[String] = condOpt(label) {
    case Identifier(name, _) ⇒ name
    case s: StringLiteral    ⇒ s.s
  }

  private def inferType(memberExpr: MemberExpr, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] = {
    val MemberExpr(target, name, _, _) = memberExpr
    for {
      targetType ← inferType(target, bindings)
      memberType ← memberLookup(targetType, name, immediateExec = immediateExec, Some(memberExpr))
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
    inferTypeBinOpExpr(leftTypeOpt, op, rightTypeOpt, right)
  }

  private def inferTypeAdd(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Seq(leftElementType)), Some(Type.Seq(rightElementType))) ⇒
        if (leftElementType == Type.Any) rightTypeOpt else leftTypeOpt
      case (Some(StringLike(_)), _) ⇒ leftTypeOpt
      case (_, Some(StringLike(_))) ⇒ rightTypeOpt
      case (Some(NumberLike(_)), _) ⇒ leftTypeOpt
      case (_, Some(NumberLike(_))) ⇒ rightTypeOpt
      case _ ⇒
        val objectAdditionTypeOpt =
          for {
            leftFields ← leftTypeOpt.flatMap(fields)
            rightFields ← rightTypeOpt.flatMap(fields)
          } yield Type.Object(leftFields ++ rightFields)
        objectAdditionTypeOpt orElse Some(Type.Instance(NumberClass))
    }

  private object ThingWithFields {
    def unapply(type_ : Type): Option[Map[String, Type]] = fields(type_)
  }

  private def fields(type_ : Type): Option[Map[String, Type]] = condOpt(type_) {
    case Type.Instance(klass)    ⇒ fields(klass)
    case Type.Object(fields)     ⇒ fields
    case Type.Generic(klass, _*) ⇒ fields(klass)
  }

  private def fields(klass: MashClass): Map[String, Type] =
    klass.fieldsMap.map { case (fieldName, field) ⇒ fieldName -> field.fieldType }

  private def inferTypeMultiply(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(NumberLike(Some(klass))), Some(NumberLike(_))) ⇒ leftTypeOpt
      case (Some(NumberLike(None)), Some(NumberLike(klassOpt))) ⇒ rightTypeOpt
      case (Some(NumberLike(_)), Some(StringLike(_) | Type.Seq(_))) ⇒ rightTypeOpt
      case (Some(StringLike(_) | Type.Seq(_)), Some(NumberLike(_))) ⇒ leftTypeOpt
      case _ ⇒ Some(Type.Instance(NumberClass))
    }

  private object StringLike {
    def unapply(typ_ : Type): Option[Type] = condOpt(typ_) {
      case Type.Tagged(StringClass, _) | Type.Instance(StringClass) ⇒ typ_
    }
  }

  private object NumberLike {
    def unapply(typ_ : Type): Option[Option[MashClass]] = condOpt(typ_) {
      case Type.Tagged(NumberClass, klass) ⇒ Some(klass)
      case Type.Instance(NumberClass)      ⇒ None
    }
  }

  private def inferTypeSubtract(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type], right: Expr): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Tagged(NumberClass, _)), _) ⇒ leftTypeOpt
      case (_, Some(Type.Tagged(NumberClass, _))) ⇒ rightTypeOpt
      case (Some(ThingWithFields(leftFields)), Some(StringLike(_))) ⇒
        right match {
          case StringLiteral(fieldName, _, false, _) ⇒ Some(Type.Object(leftFields - fieldName))
          case _                                     ⇒ None
        }
      case _ ⇒ Some(Type.Instance(NumberClass))
    }

  private def inferTypeBinOpExpr(leftTypeOpt: Option[Type], op: BinaryOperator, rightTypeOpt: Option[Type], right: Expr): Option[Type] = {
    import BinaryOperator._
    op match {
      case Plus     ⇒ inferTypeAdd(leftTypeOpt, rightTypeOpt)
      case Multiply ⇒ inferTypeMultiply(leftTypeOpt, rightTypeOpt)
      case Minus    ⇒ inferTypeSubtract(leftTypeOpt, rightTypeOpt, right)
      case Divide ⇒
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
    val InvocationExpr(function, args, _, _) = invocationExpr

    args.foreach(inferTypeArg(_, bindings))
    val typedArgs = SimpleTypedArguments.from(invocationExpr)

    inferType(function, bindings, immediateExec = false).flatMap {
      case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒
        for {
          arg ← typedArgs.positionArgs.headOption
          StringLiteral(s, _, _, _) ← Some(function)
          argType ← arg.typeOpt
          memberType ← memberLookup(argType, s, immediateExec = true)
        } yield memberType
      case Type.Seq(Type.BoundMethod(receiverType, method))    ⇒
        val strategy = method.typeInferenceStrategy
        val arguments = SimpleTypedArguments(typedArgs.arguments)
        strategy.inferTypes(new Inferencer(this, bindings), Some(receiverType), arguments).map(Type.Seq)
      case Type.Seq(elementType)                               ⇒
        Some(elementType)
      case Type.BoundMethod(receiverType, method)              ⇒
        val strategy = method.typeInferenceStrategy
        val arguments = SimpleTypedArguments(typedArgs.arguments)
        strategy.inferTypes(new Inferencer(this, bindings), Some(receiverType), arguments)
      case Type.DefinedFunction(f)                             ⇒
        val strategy = f.typeInferenceStrategy
        strategy.inferTypes(new Inferencer(this, bindings), typedArgs)
      case Type.Function(parameterModel, expr, lambdaBindings) ⇒
        val argBindings = parameterModel.bindTypes(SimpleTypedArguments(typedArgs.arguments)).boundNames
        inferType(expr, lambdaBindings ++ argBindings)
      case _                                                   ⇒
        None
    }
  }

  private def inferType(ifExpr: IfExpr, bindings: Map[String, Type]): Option[Type] = {
    val IfExpr(cond, body, elseOpt, _) = ifExpr
    inferType(cond, bindings)
    val bodyTypeOpt = inferType(body, bindings)
    val elseTypeOpt = elseOpt.flatMap(inferType(_, bindings))
    bodyTypeOpt orElse elseTypeOpt
  }

  private def memberLookup(typ: Type, klass: MashClass, name: String): Option[Type] =
    klass.fieldsMap.get(name).map(_.fieldType) orElse
      klass.getMethod(name).map { method ⇒ Type.BoundMethod(typ, method) } orElse
      klass.parentOpt.flatMap(superClass ⇒ memberLookup(typ, superClass, name))

  def memberLookup(typ: Type, name: String, immediateExec: Boolean, memberExprOpt: Option[MemberExpr] = None): Option[Type] = {
    val intermediate = typ match {
      case Type.Instance(klass)             ⇒ memberLookup(typ, klass, name)
      case Type.Tagged(baseClass, tagClass) ⇒ memberLookup(typ, baseClass, name) orElse memberLookup(typ, tagClass, name)
      case Type.Seq(elementType)            ⇒ memberLookup(typ, ListClass, name) orElse memberLookup(elementType, name, immediateExec, memberExprOpt).map(Type.Seq)
      case Type.Object(knownFields)         ⇒ knownFields.get(name) orElse memberLookup(typ, ObjectClass, name)
      case Type.DefinedFunction(_)          ⇒ memberLookup(typ, FunctionClass, name)
      case Type.BoundMethod(_, _)           ⇒ memberLookup(typ, BoundMethodClass, name)
      case Type.Generic(GroupClass, keyType, elementType) ⇒
        if (name == GroupClass.Fields.Key.name)
          Some(keyType)
        else if (name == GroupClass.Fields.Values.name)
          Some(elementType.seq)
        else
          memberLookup(typ, GroupClass, name)
      case Type.Generic(TimedResultClass, resultType) ⇒
        if (name == TimedResultClass.Fields.Result.name)
          Some(resultType)
        else
          memberLookup(typ, GroupClass, name)
      case _ ⇒ None
    }
    if (immediateExec)
      intermediate match {
        case Some(Type.BoundMethod(receiver, method)) if method.allowsNullary ⇒
          memberExprOpt.foreach(_.preInvocationTypeOpt = intermediate)
          method.typeInferenceStrategy.inferTypes(new Inferencer(this, Map()), Some(receiver), SimpleTypedArguments(Seq()))
        case Some(Type.DefinedFunction(f)) if f.allowsNullary ⇒
          memberExprOpt.foreach(_.preInvocationTypeOpt = intermediate)
          val arguments = SimpleTypedArguments(Seq())
          f.typeInferenceStrategy.inferTypes(new Inferencer(this, Map()), arguments)
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
