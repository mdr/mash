package com.github.mdr.mash.inference

import java.{ util ⇒ ju }

import com.github.mdr.mash.evaluator.{ SystemCommandFunction, _ }
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.Type.UserClassInstance
import com.github.mdr.mash.ns.collections.{ GroupClass, ListClass }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.os.{ PathClass, ProcessResultClass }
import com.github.mdr.mash.parser.AbstractSyntax.{ FunctionDeclaration, _ }
import com.github.mdr.mash.parser.{ BinaryOperator, QuotationType }
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }
import com.github.mdr.mash.utils.Utils._

import scala.PartialFunction._
import scala.collection.immutable.ListMap

case class ValueInfo(valueOpt: Option[MashValue], typeOpt: Option[Type])

object TypeInferencer {

  val ThisName = "this"

}

class TypeInferencer {

  import TypeInferencer._

  /**
    * We maintain a visited map to avoid loops in the type inference (this can happen, for example, when
    * typing fixed-point combinators)
    */
  private val visitedMap: ju.IdentityHashMap[Expr, Boolean] = new ju.IdentityHashMap

  /**
    * Attempt to infer the type of a given expression (and subexpression).
    *
    * @param bindings      -- known type information about the variable bindings available to this expression.
    * @param immediateExec -- if true, immediately "execute" (at the type level) nullary functions and methods
    */
  def inferType(expr: Expr, bindings: Map[String, Type], immediateExec: Boolean = true): Option[Type] =
    if (visitedMap.containsKey(expr))
      expr.typeOpt
    else {
      visitedMap.put(expr, true)
      try {
        val typeOpt = inferType_(expr, bindings, immediateExec)
        expr.typeOpt = typeOpt orElse Some(AnyClass)
        expr.typeBindings = bindings
        typeOpt
      } finally
        visitedMap.remove(expr)
    }

  private def inferType_(expr: Expr, bindings: Map[String, Type], immediateExec: Boolean = true): Option[Type] =
    expr match {
      case Hole(_) | PipeExpr(_, _, _)                  ⇒ None // should not be present in AST at this point
      case memberExpr: HeadlessMemberExpr               ⇒ None // should not be present in AST at this point
      case MishFunction(command, _)                     ⇒ Some(Type.BuiltinFunction(SystemCommandFunction(command)))
      case ParenExpr(body, _)                           ⇒ inferType(body, bindings)
      case BlockExpr(body, _)                           ⇒ inferType(body, bindings)
      case Literal(x, _)                                ⇒ Some(ValueTypeDetector.getType(x))
      case StringLiteral(s, QuotationType.Double, _, _) ⇒ Some(StringClass taggedWith PathClass)
      case StringLiteral(s, QuotationType.Single, _, _) ⇒ Some(StringClass)
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
      case helpExpr: HelpExpr                           ⇒ inferType(helpExpr, bindings)
      case functionDecl: FunctionDeclaration            ⇒ inferType(functionDecl, bindings)
      case classDecl: ClassDeclaration                  ⇒ inferType(classDecl, bindings)
      case lambda: LambdaExpr                           ⇒ inferType(lambda, bindings)
      case thisExpr: ThisExpr                           ⇒ bindings.get(ThisName)
    }

  private def inferType(lambdaExpr: LambdaExpr, bindings: Map[String, Type]): Option[Type] = {
    val preliminaryBindings = inferType(lambdaExpr.params, bindings)
    inferType(lambdaExpr.body, bindings ++ preliminaryBindings)
    val params = Evaluator.parameterModel(lambdaExpr.params)
    Some(Type.UserDefinedFunction(isPrivate = false, nameOpt = None, params, lambdaExpr.body, bindings))
  }

  private def inferType(params: ParamList, bindings: Map[String, Type]): Map[String, Type] =
    (for {
      param ← params.params
      paramTypeOpt = param.defaultExprOpt.flatMap(inferType(_, bindings))
      (name, type_) ← TypeParamValidationContext.bindParam(Evaluator.makeParameter(param), paramTypeOpt)
    } yield name -> type_).toMap

  private def inferType(functionDecl: FunctionDeclaration, bindings: Map[String, Type]): Option[Type] = {
    val preliminaryBodyBindings = inferType(functionDecl.params, bindings)
    inferType(functionDecl.body, bindings ++ preliminaryBodyBindings)
    Some(getFunctionType(functionDecl, bindings))
  }

  private def inferType(classDeclaration: ClassDeclaration, bindings: Map[String, Type]): Option[Type] = {
    val fieldBindings = inferType(classDeclaration.params, bindings)
    val methods = classDeclaration.bodyOpt.toSeq.flatMap(_.methods)
    val classType = getUserClassType(classDeclaration, bindings)
    val thisType = UserClassInstance(classType)
    val parentClassMethodBindings =
      for (method <- ObjectClass.methods)
        yield method.name -> Type.BoundBuiltinMethod(thisType, method)
    val initialClassBindings =
      bindings ++
        parentClassMethodBindings ++
        fieldBindings ++
        Seq(ThisName -> thisType)
    val methodBindings =
      for (method ← methods)
        yield method.name -> inferType(method, initialClassBindings).getOrElse(Type.Any)

    val updatedClassBindings = initialClassBindings ++ methodBindings

    methods.foreach(inferType(_, updatedClassBindings))

    Some(classType)
  }

  private def inferType(helpExpr: HelpExpr, bindings: Map[String, Type]): Option[Type] =
    inferType(helpExpr.expr, bindings, immediateExec = false) collect {
      case Type.BuiltinFunction(_) ⇒ FunctionHelpClass
    }

  private def inferType(statementSeq: StatementSeq, bindings: Map[String, Type]): Option[Type] = {
    var latestBindings = bindings
    for (statement <- statementSeq.statements) {
      inferType(statement, latestBindings)
      statement match {
        case AssignmentExpr(Identifier(name, _), _, _, _) ⇒
          statement.typeOpt.foreach(latestBindings += name -> _)
        case PatternAssignmentExpr(pattern, right, _)     ⇒
          latestBindings ++= TypeParamValidationContext.bindPatternParam(Evaluator.makeParamPattern(pattern), right.typeOpt)
        case functionDeclaration: FunctionDeclaration     ⇒
          latestBindings += functionDeclaration.name -> getFunctionType(functionDeclaration, latestBindings)
        case classDeclaration: ClassDeclaration           ⇒
          val userClassType = getUserClassType(classDeclaration, latestBindings)
          latestBindings += userClassType.name -> userClassType
        case _                                            ⇒
      }
    }
    statementSeq.statements.lastOption.map(_.typeOpt).getOrElse(Some(Unit))
  }

  private def getFunctionType(functionDeclaration: FunctionDeclaration,
                              bindings: Map[String, Type]): Type.UserDefinedFunction = {
    val FunctionDeclaration(attributes, _, name, paramList, body, _) = functionDeclaration
    val isPrivate = attributes.exists(_.name == Attributes.Private)
    Type.UserDefinedFunction(isPrivate, Some(name), Evaluator.parameterModel(paramList), body, bindings)
  }

  private def getUserClassType(classDeclaration: ClassDeclaration, bindings: Map[String, Type]): Type.UserClass = {
    val ClassDeclaration(_, className, paramList, bodyOpt, _) = classDeclaration

    val methods = bodyOpt.toSeq.flatMap(_.methods).map { decl ⇒
      val FunctionDeclaration(attributes, _, functionName, functionParamList, body, _) = decl
      val functionParams = Evaluator.parameterModel(functionParamList)
      val isPrivate = attributes.exists(_.name == Attributes.Private)
      val methodType = Type.UserDefinedFunction(isPrivate, Some(functionName), functionParams, body, bindings)
      functionName -> methodType
    }
    val classParams = Evaluator.parameterModel(paramList)
    Type.UserClass(className, classParams, ListMap(methods: _*))
  }

  private def inferType(assignmentExpr: AssignmentExpr, bindings: Map[String, Type]): Option[Type] = {
    val AssignmentExpr(left, operatorOpt, right, _) = assignmentExpr
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
    val elementTypes = listExpr.elements.flatMap(inferType(_, bindings))
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
      case ExprPart(expr) ⇒ inferType(expr, bindings)
      case StringPart(s)  ⇒
    }
    Some(StringClass taggedWith PathClass)
  }

  private def inferType(identifier: Identifier, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] =
    bindings.get(identifier.name).when(immediateExec, inferImmediateExec(_, Some(identifier)))

  private def inferType(name: String, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] =
    bindings.get(name).flatMap {
      case typ@Type.BuiltinFunction(f) if f.allowsNullary && immediateExec ⇒
        f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), TypedArguments())
      case x                                                               ⇒
        Some(x)
    }

  private def inferType(objectExpr: ObjectExpr, bindings: Map[String, Type]): Option[Type] = {
    val ObjectExpr(entries, _) = objectExpr
    val fieldTypes =
      entries.flatMap {
        case FullObjectEntry(fieldExpr, valueExpr, _) ⇒
          inferType(fieldExpr, bindings)
          for {
            label ← getFieldName(fieldExpr)
            type_ = inferType(valueExpr, bindings).getOrElse(Type.Any)
          } yield label -> type_
        case ShorthandObjectEntry(field, _)           ⇒
          Seq(field -> inferType(field, bindings, immediateExec = true).getOrElse(Type.Any))
      }
    Some(Type.Object(fieldTypes.toMap))
  }

  private def getFieldName(label: Expr): Option[String] = condOpt(label) {
    case Identifier(name, _) ⇒ name
    case s: StringLiteral    ⇒ s.s
  }

  private def inferType(memberExpr: MemberExpr, bindings: Map[String, Type], immediateExec: Boolean): Option[Type] = {
    for {
      targetType ← inferType(memberExpr.target, bindings)
      memberType ← memberLookup(targetType, memberExpr.name, immediateExec = immediateExec, Some(memberExpr), Some(memberExpr.target))
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
      case (Some(StringLike(_)), _)                                            ⇒ leftTypeOpt
      case (_, Some(StringLike(_)))                                            ⇒ rightTypeOpt
      case (Some(NumberLike(_)), _)                                            ⇒ leftTypeOpt
      case (_, Some(NumberLike(_)))                                            ⇒ rightTypeOpt
      case _                                                                   ⇒
        val objectAdditionTypeOpt =
          for {
            leftFields ← leftTypeOpt.flatMap(getObjectFields)
            rightFields ← rightTypeOpt.flatMap(getObjectFields)
          } yield Type.Object(leftFields ++ rightFields)
        objectAdditionTypeOpt orElse Some(Type.Instance(NumberClass))
    }

  private object ThingWithFields {
    def unapply(type_ : Type): Option[Map[String, Type]] = getObjectFields(type_)
  }

  private def getObjectFields(type_ : Type): Option[Map[String, Type]] = condOpt(type_) {
    case Type.Instance(klass)    ⇒ fields(klass)
    case Type.Object(fields)     ⇒ fields
    case Type.Generic(klass, _*) ⇒ fields(klass)
  }

  private def fields(klass: MashClass): Map[String, Type] =
    klass.fieldsMap.map { case (fieldName, field) ⇒ fieldName -> field.fieldType }

  private def inferTypeMultiply(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(NumberLike(Some(klass))), Some(NumberLike(_)))     ⇒ leftTypeOpt
      case (Some(NumberLike(None)), Some(NumberLike(klassOpt)))     ⇒ rightTypeOpt
      case (Some(NumberLike(_)), Some(StringLike(_) | Type.Seq(_))) ⇒ rightTypeOpt
      case (Some(StringLike(_) | Type.Seq(_)), Some(NumberLike(_))) ⇒ leftTypeOpt
      case _                                                        ⇒ Some(Type.Instance(NumberClass))
    }

  private object StringLike {
    def unapply(type_ : Type): Option[Type] = condOpt(type_) {
      case Type.Tagged(StringClass, _) | Type.Instance(StringClass) ⇒ type_
    }
  }

  private object NumberLike {
    def unapply(type_ : Type): Option[Option[MashClass]] = condOpt(type_) {
      case Type.Tagged(NumberClass, klass) ⇒ Some(klass)
      case Type.Instance(NumberClass)      ⇒ None
    }
  }

  private def inferTypeSubtract(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type], right: Expr): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Tagged(NumberClass, _)), _)                             ⇒ leftTypeOpt
      case (_, Some(Type.Tagged(NumberClass, _)))                             ⇒ rightTypeOpt
      case (Some(ThingWithFields(leftFields)), Some(StringLike(_)))           ⇒
        right.constantValueOpt collect {
          case MashString(fieldName, _) ⇒ Type.Object(leftFields - fieldName)
        }
      case (Some(ThingWithFields(leftFields)), Some(Type.Seq(_))) ⇒
        right.constantValueOpt collect {
          case xs: MashList ⇒
            val removedFields = xs.elements.collect { case MashString(s, _) ⇒ s }.toSet
            Type.Object(leftFields.filterKeys(k ⇒ !removedFields.contains(k)))
        }
      case (Some(Type.Seq(elementType)), _)                                   ⇒ leftTypeOpt
      case (_, Some(Type.Seq(elementType)))                                   ⇒ rightTypeOpt
      case _                                                                  ⇒ Some(NumberClass)
    }

  private def inferTypeBinOpExpr(leftTypeOpt: Option[Type], op: BinaryOperator, rightTypeOpt: Option[Type], right: Expr): Option[Type] = {
    import BinaryOperator._
    op match {
      case Plus                                                                             ⇒ inferTypeAdd(leftTypeOpt, rightTypeOpt)
      case Multiply                                                                         ⇒ inferTypeMultiply(leftTypeOpt, rightTypeOpt)
      case Minus                                                                            ⇒ inferTypeSubtract(leftTypeOpt, rightTypeOpt, right)
      case Divide                                                                           ⇒
        (leftTypeOpt, rightTypeOpt) match {
          case (Some(Type.Tagged(NumberClass, _)), _) ⇒ leftTypeOpt
          case (_, Some(Type.Tagged(NumberClass, _))) ⇒ rightTypeOpt
          case _                                      ⇒ Some(NumberClass)
        }
      case Equals | NotEquals | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals ⇒
        Some(BooleanClass)
      case Sequence                                                                         ⇒
        rightTypeOpt
      case And | Or                                                                         ⇒
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

  private def inferTypeInvocation(functionType: Type,
                                  typedArgs: TypedArguments,
                                  function: Expr,
                                  bindings: Map[String, Type]): Option[Type] = functionType match {
    case Type.Instance(StringClass) | Type.Tagged(StringClass, _)                                                      ⇒
      for {
        arg ← typedArgs.positionArgs.headOption
        MashString(s, _) ← function.constantValueOpt
        argType ← arg.typeOpt
        memberType ← memberLookup(argType, s, immediateExec = true)
      } yield memberType
    case Type.Seq(elementType: Type.BoundUserDefinedMethod)                                                            ⇒
      inferTypeInvocation(elementType, typedArgs, function, bindings).map(Type.Seq)
    case Type.Seq(elementType: Type.BoundBuiltinMethod)                                                                ⇒
      inferTypeInvocation(elementType, typedArgs, function, bindings).map(Type.Seq)
    case Type.Seq(elementType)                                                                                         ⇒
      Some(elementType)
    case Type.BoundUserDefinedMethod(targetType, Type.UserDefinedFunction(_, _, parameterModel, body, methodBindings)) ⇒
      val argBindings = parameterModel.bindTypes(TypedArguments(typedArgs.arguments)).boundNames
      inferType(body, methodBindings ++ argBindings ++ Seq(ThisName -> targetType))
    case Type.BoundBuiltinMethod(targetType, method)                                                                   ⇒
      val strategy = method.typeInferenceStrategy
      val arguments = TypedArguments(typedArgs.arguments)
      strategy.inferTypes(new Inferencer(this, bindings), Some(targetType), arguments)
    case Type.BuiltinFunction(f)                                                                                       ⇒
      f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), typedArgs)
    case Type.UserDefinedFunction(_, _, parameterModel, expr, lambdaBindings)                                          ⇒
      val argBindings = parameterModel.bindTypes(TypedArguments(typedArgs.arguments)).boundNames
      inferType(expr, lambdaBindings ++ argBindings)
    case Type.Instance(ClassClass)                                                                                     ⇒
      getStaticMethodType(function, MashClass.ConstructorMethodName).flatMap { case Type.BuiltinFunction(f) ⇒
        f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), typedArgs)
      }
    case userClass: Type.UserClass                                                                                     ⇒
      val Type.BuiltinFunction(constructor) = getConstructor(userClass)
      constructor.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), typedArgs)
    case _                                                                                                             ⇒
      None
  }

  private def inferTypeInvocationExpr(invocationExpr: InvocationExpr, bindings: Map[String, Type]): Option[Type] = {
    val InvocationExpr(function, args, _, _) = invocationExpr

    args.foreach(inferTypeArg(_, bindings))
    val typedArgs = TypedArguments.from(invocationExpr)

    inferType(function, bindings, immediateExec = false).flatMap(inferTypeInvocation(_, typedArgs, function, bindings))
  }

  private def inferType(ifExpr: IfExpr, bindings: Map[String, Type]): Option[Type] = {
    val IfExpr(cond, body, elseOpt, _) = ifExpr
    inferType(cond, bindings)
    val bodyTypeOpt = inferType(body, bindings)
    val elseTypeOpt = elseOpt.flatMap(inferType(_, bindings))
    bodyTypeOpt orElse elseTypeOpt
  }

  private def memberLookup(targetType: Type, klass: MashClass, name: String): Option[Type] =
    klass.fieldsMap.get(name).map(_.fieldType) orElse
      klass.getMethod(name).map(getMethodType(targetType, _)) orElse
      klass.parentOpt.flatMap(superClass ⇒ memberLookup(targetType, superClass, name))

  private def getMethodType(targetType: Type, method: MashMethod) = method match {
    case UserDefinedMethod(_, name, params, _, body, context, isPrivate) ⇒
      val bindings = new ValueTypeDetector().buildBindings(context.scopeStack.bindings)
      val functionType = Type.UserDefinedFunction(isPrivate, Some(name), params, body, bindings)
      Type.BoundUserDefinedMethod(targetType, functionType)
    case _                                                               ⇒
      Type.BoundBuiltinMethod(targetType, method)
  }

  private def getStaticMethodType(targetExpr: Expr, name: String): Option[Type.BuiltinFunction] =
    targetExpr
      .constantValueOpt
      .flatMap(getStaticMethod(_, name))
      .map(Type.BuiltinFunction)

  private def getStaticMethod(value: MashValue, name: String): Option[MashFunction] =
    condOpt(value) { case klass: MashClass ⇒ klass }.flatMap(_ getStaticMethod name)

  private def memberLookup(genericType: Type.Generic,
                           name: String): Option[Type] = genericType match {
    case Type.Generic(GroupClass, keyType, elementType) ⇒
      if (name == GroupClass.Fields.Key.name)
        Some(keyType)
      else if (name == GroupClass.Fields.Values.name)
        Some(elementType.seq)
      else
        memberLookup(genericType, GroupClass, name)
    case Type.Generic(TimedResultClass, resultType)     ⇒
      if (name == TimedResultClass.Fields.Result.name)
        Some(resultType)
      else
        memberLookup(genericType, TimedResultClass, name)
    case _                                              ⇒
      None
  }

  private def getConstructor(userClass: Type.UserClass): Type.BuiltinFunction = {

    object FakeFunction extends MashFunction(MashClass.ConstructorMethodName) {

      override def apply(arguments: Arguments): MashValue =
        throw new AssertionError("Fake function cannot be executed")

      override def summaryOpt = Some(s"Constructor for ${userClass.name}")

      override def params: ParameterModel = userClass.params

      override def typeInferenceStrategy =
        (inferencer: Inferencer, arguments: TypedArguments) => Some(UserClassInstance(userClass))
    }

    Type.BuiltinFunction(FakeFunction)
  }

  def memberLookup(targetType: Type,
                   name: String,
                   immediateExec: Boolean,
                   memberExprOpt: Option[MemberExpr] = None,
                   targetExprOpt: Option[Expr] = None): Option[Type] = {
    val rawType = targetType match {
      case Type.Instance(ClassClass)                 ⇒ targetExprOpt.flatMap(getStaticMethodType(_, name)) orElse memberLookup(targetType, ClassClass, name)
      case userClass: Type.UserClass                 ⇒ if (name == MashClass.ConstructorMethodName) Some(getConstructor(userClass)) else memberLookup(targetType, ClassClass, name)
      case Type.Instance(klass)                      ⇒ memberLookup(targetType, klass, name)
      case userClassInstance: Type.UserClassInstance ⇒ memberLookup(userClassInstance, name)
      case Type.Tagged(baseClass, tagClass)          ⇒ memberLookup(targetType, baseClass, name) orElse memberLookup(targetType, tagClass, name)
      case Type.Seq(elementType)                     ⇒ memberLookup(targetType, ListClass, name) orElse memberLookup(elementType, name, immediateExec, memberExprOpt, targetExprOpt).map(Type.Seq)
      case Type.Object(knownFields)                  ⇒ knownFields.get(name) orElse memberLookup(targetType, ObjectClass, name)
      case Type.BuiltinFunction(_)                   ⇒ memberLookup(targetType, FunctionClass, name)
      case Type.UserDefinedFunction(_, _, _, _, _)   ⇒ memberLookup(targetType, FunctionClass, name)
      case Type.BoundUserDefinedMethod(_, _)         ⇒ memberLookup(targetType, BoundMethodClass, name)
      case Type.BoundBuiltinMethod(_, _)             ⇒ memberLookup(targetType, BoundMethodClass, name)
      case genericType: Type.Generic                 ⇒ memberLookup(genericType, name)
      case _                                         ⇒ None
    }
    rawType.when(immediateExec, inferImmediateExec(_, memberExprOpt))
  }

  private def memberLookup(userClassInstance: Type.UserClassInstance, name: String): Option[Type] = {
    val Type.UserClassInstance(Type.UserClass(_, params, methods)) = userClassInstance

    val fieldLookup = params.params.find(_.nameOpt contains name).map(_ ⇒ Type.Instance(AnyClass))
    val methodLookup = methods.get(name).map(Type.BoundUserDefinedMethod(userClassInstance, _))
    fieldLookup orElse
      methodLookup orElse
      memberLookup(userClassInstance, ObjectClass, name)
  }

  /**
    * Infer the type of an immediately-invoked nullary expression
    */
  private def inferImmediateExec(intermediateTypeOpt: Option[Type], exprOpt: Option[Expr]): Option[Type] =
    intermediateTypeOpt match {
      case Some(Type.BuiltinFunction(f)) if f.allowsNullary                                             ⇒
        exprOpt.foreach(_.preInvocationTypeOpt = intermediateTypeOpt)
        f.typeInferenceStrategy.inferTypes(new Inferencer(this, Map()), TypedArguments())
      case Some(Type.BoundBuiltinMethod(targetType, method)) if method.allowsNullary                    ⇒
        exprOpt.foreach(_.preInvocationTypeOpt = intermediateTypeOpt)
        method.typeInferenceStrategy.inferTypes(new Inferencer(this, Map()), Some(targetType), TypedArguments())
      case Some(Type.UserDefinedFunction(_, _, params, body, functionBindings)) if params.allowsNullary ⇒
        exprOpt.foreach(_.preInvocationTypeOpt = intermediateTypeOpt)
        val argBindings = params.bindTypes(TypedArguments()).boundNames
        inferType(body, functionBindings ++ argBindings)
      case Some(Type.BoundUserDefinedMethod(targetType, function)) if function.params.allowsNullary     ⇒
        val Type.UserDefinedFunction(_, _, params, body, methodBindings) = function
        exprOpt.foreach(_.preInvocationTypeOpt = intermediateTypeOpt)
        val argBindings = params.bindTypes(TypedArguments()).boundNames
        inferType(body, methodBindings ++ argBindings ++ Seq(ThisName -> targetType))
      case x                                                                                            ⇒
        x
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
      case (Some(Type.Seq(elementType)), Some(Type.Instance(NumberClass)))                  ⇒ elementType
      case (Some(Type.Instance(StringClass)), Some(Type.Instance(NumberClass)))             ⇒ Type.Instance(StringClass)
      case (Some(taggedType@Type.Tagged(StringClass, _)), Some(Type.Instance(NumberClass))) ⇒ taggedType
    }
  }

}
