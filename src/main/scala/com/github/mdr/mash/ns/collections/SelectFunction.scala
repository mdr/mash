package com.github.mdr.mash.ns.collections

import scala.collection.immutable.ListMap

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import com.github.mdr.mash.parser.AbstractSyntax._

/**
 * Select members of an object
 */
object SelectFunction extends MashFunction("collections.select") {

  private val AddShortFlag = 'a'

  object Params {
    val Target = Parameter(
      name = "target",
      summary = "Either an object or sequence of objects to select fields from",
      isLast = true)
    val Add = Parameter(
      name = "add",
      summary = "Add the fields to the existing set of members, rather than replacing",
      shortFlagOpt = Some(AddShortFlag),
      defaultValueGeneratorOpt = Some(() ⇒ false),
      isFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Target, Add))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments, ignoreAdditionalParameters = true)
    val add = Truthiness.isTruthy(boundParams(Add))
    val target = boundParams(Target)
    val fieldsAndFunctions: Seq[(String, Any ⇒ Any)] = arguments.evaluatedArguments.init.flatMap {
      case EvaluatedArgument.PositionArg(value, _) ⇒
        value match {
          case MashString(s, _) ⇒ Some(s -> FunctionHelpers.interpretAsFunction(value))
          case _                ⇒ throw new EvaluatorException("Positional arguments must be strings")
        }
      case EvaluatedArgument.ShortFlag(flags, _) ⇒
        if (flags == Seq(AddShortFlag.toString))
          None
        else
          throw new EvaluatorException("Short flags not supported by select")
      case EvaluatedArgument.LongFlag(flag, None, _) ⇒
        if (flag == Add.name)
          None
        else
          Some(flag -> FunctionHelpers.interpretAsFunction(MashString(flag)))
      case EvaluatedArgument.LongFlag(flag, Some(value), _) ⇒
        Some(flag -> FunctionHelpers.interpretAsFunction(value))
    }
    target match {
      case xs: Seq[_] ⇒ xs.map(doSelect(_, fieldsAndFunctions, add))
      case x          ⇒ doSelect(x, fieldsAndFunctions, add)
    }
  }

  private def doSelect(target: Any, fieldsAndFunctions: Seq[(String, Any ⇒ Any)], add: Boolean) = {
    val baseFields: ListMap[String, Any] =
      if (add) {
        target match {
          case MashObject(fields, _) ⇒ ListMap(fields.toSeq: _*)
          case _                     ⇒ ListMap()
        }
      } else
        ListMap()
    val mapPairs =
      for ((field, f) ← fieldsAndFunctions)
        yield field -> f(target)
    val newFields = ListMap((mapPairs): _*)
    MashObject(baseFields ++ newFields, classOpt = None)
  }

  override def typeInferenceStrategy = SelectTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    if (argPos < arguments.arguments.size - 1)
      arguments.arguments.last match {
        case TypedArgument.PositionArg(expr) ⇒
          expr.typeOpt match {
            case Some(Type.Seq(elementType)) ⇒ Seq(CompletionSpec.Members(elementType))
            case Some(targetType)            ⇒ Seq(CompletionSpec.Members(targetType))
            case _                           ⇒ Seq()
          }
        case _ ⇒ Seq()
      }
    else
      Seq()

  override def summary = "Select members of an object or sequence of objects"

  override def descriptionOpt = Some("""If the input is a sequence, the selection is applied to every element and a 
  sequence of new objects is returned.
If the input is a single object, a single new object is returned with the selection applied.

The arguments are interpreted as follows:
  1) A positional string argument -- the input object member with that name is copied to an output field with 
    the same name.
  2) A flag with no argument -- the input member with the same name as the flag is copied to an output field with
   the same name. 
  3) A flag with an argument -- the flag argument is applied as a function to the input object, and the result 
  is copied to an output field with the flag name.

The order of fields in the argument list is retained in the new output objects.

Examples:
  select "foo" { foo: 42, bar: 24 }              # { foo: 42 }
  select --foo { foo: 42, bar: 24 }              # { foo: 42 }
  select --foo { foo: 42, bar: 24 }              # { foo: 42 }
  select --baz=(_.foo * 2) { foo: 42, bar: 24 }  # { baz: 84 }
  select "bar" "foo" { foo: 42, bar: 24 }        # { bar: 24, foo: 42 }
  select --add --baz=(_.foo * 2) { foo: 42 }     # { foo: 42, baz: 84 }
  select "foo" [{ foo: 42, bar: 24 }, { foo: 12, bar: 18 }] # [{ foo: 42 }, { foo: 12 }]""")
}

