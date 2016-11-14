package com.github.mdr.mash.evaluator

import java.time.Instant
import java.time.LocalDate
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.ns.time.LocalDateClass
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.utils.Utils
import scala.PartialFunction.condOpt

object MemberEvaluator extends EvaluatorHelper {

  case class MemberExprEvalResult(result: MashValue, wasVectorised: Boolean)

  def evaluateMemberExpr(memberExpr: MemberExpr, immediatelyResolveNullaryWhenVectorising: Boolean)(implicit context: EvaluationContext): MemberExprEvalResult = {
    val MemberExpr(expr, name, isNullSafe, sourceInfoOpt) = memberExpr
    val target = Evaluator.evaluate(expr)
    evaluateMemberExpr_(memberExpr, target, immediatelyResolveNullaryWhenVectorising)
  }

  def evaluateMemberExpr_(memberExpr: AbstractMemberExpr, target: MashValue, immediatelyResolveNullaryWhenVectorising: Boolean)(implicit context: EvaluationContext): MemberExprEvalResult = {
    val name = memberExpr.name
    val isNullSafe = memberExpr.isNullSafe
    val locationOpt = memberExpr.sourceInfoOpt.flatMap(info ⇒ condOpt(info.expr) {
      case ConcreteSyntax.MemberExpr(_, _, name) ⇒ SourceLocation(info.provenance, PointedRegion(name.offset, name.region))
    })
    if (target == MashNull && isNullSafe)
      MemberExprEvalResult(MashNull, wasVectorised = false)
    else {
      lazy val scalarLookup = MemberEvaluator.maybeLookup(target, name).map(x ⇒ MemberExprEvalResult(x, wasVectorised = false))
      lazy val vectorisedLookup = vectorisedMemberLookup(target, name, isNullSafe, immediatelyResolveNullaryWhenVectorising, locationOpt).map(
        x ⇒ MemberExprEvalResult(x, wasVectorised = true))
      scalarLookup orElse vectorisedLookup getOrElse (throw new EvaluatorException(s"Cannot find member '$name' in value of type ${target.typeName}", locationOpt))
    }
  }

  private def vectorisedMemberLookup(target: MashValue, name: String, isNullSafe: Boolean, immediatelyResolveNullaryWhenVectorising: Boolean, locationOpt: Option[SourceLocation]): Option[MashList] =
    target match {
      case xs: MashList ⇒
        val options = xs.items.map {
          case MashNull if isNullSafe ⇒ Some(MashNull)
          case x ⇒
            val lookupOpt = MemberEvaluator.maybeLookup(x, name)
            if (immediatelyResolveNullaryWhenVectorising)
              lookupOpt.map(lookup ⇒ Evaluator.immediatelyResolveNullaryFunctions(lookup, locationOpt))
            else
              lookupOpt
        }
        Utils.sequence(options).map(MashList(_))
      case _ ⇒
        None
    }

  private def lookupMethod(target: MashValue, klass: MashClass, name: String): Option[MashValue] = {
    val directResultOpt =
      for {
        method ← klass.getMethod(name)
      } yield BoundMethod(target, method, klass)
    lazy val parentResultOpt = klass.parentOpt.flatMap(parentClass ⇒ lookupMethod(target, parentClass, name))
    directResultOpt orElse parentResultOpt
  }

  private def lookupMethod(target: MashObject, name: String): Option[MashValue] =
    for {
      klass ← target.classOpt orElse Some(ObjectClass)
      boundMethod ← lookupMethod(target, klass, name)
    } yield boundMethod

  def lookup(target: MashValue, field: Field): MashValue =
    lookup(target, field.name)

  def lookup(target: MashValue, name: String, locationOpt: Option[SourceLocation] = None): MashValue =
    maybeLookup(target, name).getOrElse(
      throw new EvaluatorException(s"Cannot find member '$name' in value of type ${target.typeName}", locationOpt))

  def hasMember(target: MashValue, name: String): Boolean =
    maybeLookup(target, name).isDefined

  def maybeLookup(target: MashValue, name: String): Option[MashValue] =
    target match {
      case MashNumber(n, tagClassOpt)       ⇒ lookupMethod(target, NumberClass, name) orElse tagClassOpt.flatMap(tag ⇒ lookupMethod(target, tag, name))
      case MashString(s, tagClassOpt)       ⇒ lookupMethod(target, StringClass, name) orElse tagClassOpt.flatMap(tag ⇒ lookupMethod(target, tag, name))
      case MashNull                         ⇒ lookupMethod(target, NullClass, name)
      case MashUnit                         ⇒ lookupMethod(target, UnitClass, name)
      case b: MashBoolean                   ⇒ lookupMethod(b, BooleanClass, name)
      case xs: MashList                     ⇒ lookupMethod(xs, ListClass, name)
      case f: MashFunction                  ⇒ lookupMethod(f, FunctionClass, name)
      case bm: BoundMethod                  ⇒ lookupMethod(bm, BoundMethodClass, name)
      case klass: MashClass                 ⇒ lookupMethod(klass, ClassClass, name)
      case dt @ MashWrapped(_: Instant)     ⇒ lookupMethod(dt, DateTimeClass, name)
      case date @ MashWrapped(_: LocalDate) ⇒ lookupMethod(date, LocalDateClass, name)
      case obj: MashObject                  ⇒ obj.get(name) orElse lookupMethod(obj, name)
      case _                                ⇒ None
    }

}