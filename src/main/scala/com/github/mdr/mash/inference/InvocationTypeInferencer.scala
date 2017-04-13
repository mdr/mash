package com.github.mdr.mash.inference

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.BooleanFunction
import com.github.mdr.mash.ns.core.{ BooleanClass, ClassClass }
import com.github.mdr.mash.parser.AbstractSyntax.{ Argument, Expr, InvocationExpr }
import com.github.mdr.mash.runtime.MashString
import TypeInferencer._

trait InvocationTypeInferencer { self: TypeInferencer ⇒


  protected def inferTypeInvocation(functionType: Type,
                                  typedArgs: TypedArguments,
                                  function: Expr,
                                  bindings: Map[String, Type]): Option[Type] = functionType match {
    case Type.Patterns.AnyString(_)                                              ⇒
      for {
        arg ← typedArgs.positionArgs.headOption
        MashString(s, _) ← function.constantValueOpt
        argType ← arg.typeOpt
        memberType ← memberLookup(argType, s, immediateExec = true)
      } yield memberType
    case Type.Seq(elementType: Type.BoundUserDefinedMethod)                      ⇒
      inferTypeInvocation(elementType, typedArgs, function, bindings).map(Type.Seq)
    case Type.Seq(elementType: Type.BoundBuiltinMethod)                          ⇒
      inferTypeInvocation(elementType, typedArgs, function, bindings).map(Type.Seq)
    case Type.Seq(elementType)                                                   ⇒
      Some(elementType)
    case Type.BoundUserDefinedMethod(targetType, userDefinedFunction)            ⇒
      val Type.UserDefinedFunction(_, _, _, parameterModel, body, methodBindings) = userDefinedFunction
      val argBindings = parameterModel.bindTypes(typedArgs).boundNames
      inferType(body, methodBindings ++ argBindings ++ Seq(ThisName -> targetType))
    case Type.BoundBuiltinMethod(targetType, method)                             ⇒
      val strategy = method.typeInferenceStrategy
      val arguments = TypedArguments(typedArgs.arguments)
      strategy.inferTypes(new Inferencer(this, bindings), Some(targetType), arguments)
    case Type.BuiltinFunction(f)                                                 ⇒
      f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), typedArgs)
    case Type.UserDefinedFunction(_, _, _, parameterModel, expr, lambdaBindings) ⇒
      val argBindings = parameterModel.bindTypes(typedArgs).boundNames
      inferType(expr, lambdaBindings ++ argBindings)
    case Type.Instance(ClassClass)                                               ⇒
      getStaticMethodType(function, MashClass.ConstructorMethodName).flatMap { case Type.BuiltinFunction(f) ⇒
        f.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), typedArgs)
      }
    case Type.Instance(BooleanClass)                                             ⇒
      val dummyFunction = new BooleanFunction(true)
      val argBindings = dummyFunction.params.bindTypes(typedArgs)
      argBindings.getType(dummyFunction.Params.Then) orElse argBindings.getType(dummyFunction.Params.Else)
    case userClass: Type.UserClass                                               ⇒
      val Type.BuiltinFunction(constructor) = getConstructor(userClass)
      constructor.typeInferenceStrategy.inferTypes(new Inferencer(this, bindings), typedArgs)
    case _                                                                       ⇒
      None
  }

  protected def inferTypeInvocationExpr(invocationExpr: InvocationExpr, bindings: Map[String, Type]): Option[Type] = {
    val InvocationExpr(function, args, _, _) = invocationExpr

    args.foreach(inferTypeArg(_, bindings))
    val typedArgs = TypedArguments.from(invocationExpr)

    inferType(function, bindings, immediateExec = false).flatMap(inferTypeInvocation(_, typedArgs, function, bindings))
  }

  private def inferTypeArg(arg: Argument, bindings: Map[String, Type]) {
    arg match {
      case Argument.PositionArg(e, _)              ⇒ inferType(e, bindings)
      case Argument.LongFlag(flag, Some(value), _) ⇒ inferType(value, bindings)
      case _                                       ⇒
    }
  }

}
