package com.github.mdr.mash.evaluator

import java.time.{ Instant, LocalDate }

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateTimeClass, DateClass }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.{ PointedRegion, Utils }

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
    evalIt2(target, name, immediatelyResolveNullaryWhenVectorising, isNullSafe, locationOpt)
  }

  private def evalIt2(target: MashValue, name: String, immediatelyResolveNullaryWhenVectorising: Boolean, isNullSafe: Boolean, locationOpt: Option[SourceLocation]): MemberExprEvalResult = {
    if (target == MashNull && isNullSafe)
      MemberExprEvalResult(MashNull, wasVectorised = false)
    else {
      def scalarLookup = MemberEvaluator.maybeLookup(target, name).map(x ⇒ MemberExprEvalResult(x, wasVectorised = false))
      def vectorisedLookup = vectorisedMemberLookup(target, name, isNullSafe, immediatelyResolveNullaryWhenVectorising, locationOpt).map(
        x ⇒ MemberExprEvalResult(x, wasVectorised = true))
      scalarLookup orElse vectorisedLookup getOrElse {
        attemptFix(name, namesFrom(target)) match {
          case Some(fixedName) if fixedName != name ⇒
            println(s"Fixing $name to $fixedName")
            evalIt2(target, fixedName, immediatelyResolveNullaryWhenVectorising, isNullSafe, locationOpt)
          case None                                 ⇒
            throw new EvaluatorException(s"Cannot find member '$name' in value of type ${target.typeName}", locationOpt)
        }
      }
    }
  }

  private def vectorisedMemberLookup(target: MashValue, name: String, isNullSafe: Boolean, immediatelyResolveNullaryWhenVectorising: Boolean, locationOpt: Option[SourceLocation]): Option[MashList] =
    target match {
      case xs: MashList ⇒
        val options = xs.elements.map {
          case MashNull if isNullSafe ⇒ Some(MashNull)
          case x                      ⇒
            val lookupOpt = MemberEvaluator.maybeLookup(x, name)
            if (immediatelyResolveNullaryWhenVectorising)
              lookupOpt.map(lookup ⇒ Evaluator.immediatelyResolveNullaryFunctions(lookup, locationOpt))
            else
              lookupOpt
        }
        Utils.sequence(options).map(MashList(_))
      case _            ⇒
        None
    }

  private def lookupMethod(target: MashValue, klass: MashClass, name: String): Option[BoundMethod] = {
    val directResultOpt =
      for {
        method ← klass.getMethod(name)
      } yield BoundMethod(target, method, klass)
    def parentResultOpt = klass.parentOpt.flatMap(parentClass ⇒ lookupMethod(target, parentClass, name))
    directResultOpt orElse parentResultOpt
  }

  def lookup(target: MashValue, field: Field): MashValue =
    lookup(target, field.name)

  private def namesFrom(target: MashValue): Seq[String] = (target match {
    case MashNumber(n, tagClassOpt)     ⇒ NumberClass.memberNames ++ tagClassOpt.toSeq.flatMap(_.memberNames)
    case MashString(s, tagClassOpt)     ⇒ StringClass.memberNames ++ tagClassOpt.toSeq.flatMap(_.memberNames)
    case MashNull                       ⇒ NullClass.memberNames
    case MashUnit                       ⇒ UnitClass.memberNames
    case b: MashBoolean                 ⇒ BooleanClass.memberNames
    case xs: MashList                   ⇒ ListClass.memberNames
    case f: MashFunction                ⇒ FunctionClass.memberNames
    case bm: BoundMethod                ⇒ BoundMethodClass.memberNames
    case klass: MashClass               ⇒ ClassClass.memberNames
    case dt@MashWrapped(_: Instant)     ⇒ DateTimeClass.memberNames
    case date@MashWrapped(_: LocalDate) ⇒ DateClass.memberNames
    case obj: MashObject                ⇒ obj.immutableFields.keys.toSeq ++ (obj.classOpt getOrElse ObjectClass).memberNames
    case _                              ⇒ Seq()
  }) ++ AnyClass.memberNames

  def lookup(target: MashValue, name: String, locationOpt: Option[SourceLocation] = None): MashValue =
    maybeLookup(target, name).getOrElse {
      attemptFix(name, namesFrom(target)) match {
        case Some(fixedName) if fixedName != name ⇒
          println(s"Fixing $name to $fixedName")
          lookup(target, fixedName, locationOpt)
        case None                                 ⇒
          throw new EvaluatorException(s"Cannot find member '$name' in value of type ${target.typeName}", locationOpt)
      }
    }

  private def attemptFix(name: String, candidates: Seq[String]): Option[String] =
    if (candidates.isEmpty)
      None
    else
      Some(candidates.minBy(org.apache.commons.lang3.StringUtils.getLevenshteinDistance(name, _)))

  def hasMember(target: MashValue, name: String): Boolean =
    maybeLookup(target, name).isDefined

  /**
    * @return a bound method, a static method, or a field value corresponding to the given name in the target
    */
  def maybeLookup(target: MashValue, name: String): Option[MashValue] =
    target match {
      case MashNumber(n, tagClassOpt)     ⇒ lookupMethod(target, NumberClass, name) orElse tagClassOpt.flatMap(lookupMethod(target, _, name))
      case MashString(s, tagClassOpt)     ⇒ lookupMethod(target, StringClass, name) orElse tagClassOpt.flatMap(lookupMethod(target, _, name))
      case MashNull                       ⇒ lookupMethod(target, NullClass, name)
      case MashUnit                       ⇒ lookupMethod(target, UnitClass, name)
      case b: MashBoolean                 ⇒ lookupMethod(b, BooleanClass, name)
      case xs: MashList                   ⇒ lookupMethod(xs, ListClass, name)
      case f: MashFunction                ⇒ lookupMethod(f, FunctionClass, name)
      case bm: BoundMethod                ⇒ lookupMethod(bm, BoundMethodClass, name)
      case klass: MashClass               ⇒ klass.getStaticMethod(name) orElse lookupMethod(klass, ClassClass, name)
      case dt@MashWrapped(_: Instant)     ⇒ lookupMethod(dt, DateTimeClass, name)
      case date@MashWrapped(_: LocalDate) ⇒ lookupMethod(date, DateClass, name)
      case obj: MashObject                ⇒ obj.get(name) orElse lookupMethod(obj, obj.classOpt getOrElse ObjectClass, name)
      case _                              ⇒ None
    }

}