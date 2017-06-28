package com.github.mdr.mash.evaluator

import java.time.{ Instant, LocalDate }

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.{ PointedRegion, Utils }

import scala.PartialFunction.condOpt

object MemberEvaluator extends EvaluatorHelper {

  case class MemberExprEvalResult(result: MashValue, wasVectorised: Boolean = false)

  /**
    * @param invokeNullaryWhenVectorising if true, then any vectorised member lookups that result in
    *                                     nullary-callable functions will be invoked. If false, they will
    *                                     be returned as-is. (false is useful for vectorised invocations).
    */
  def evaluateMemberExpr(memberExpr: MemberExpr,
                         invokeNullaryWhenVectorising: Boolean)(implicit context: EvaluationContext): MemberExprEvalResult = {
    val target = Evaluator.evaluate(memberExpr.target)
    val thisTarget = memberExpr.target.isInstanceOf[ThisExpr]
    evaluateMemberExpr(memberExpr, target, thisTarget, invokeNullaryWhenVectorising)
  }

  def evaluateMemberExpr(memberExpr: AbstractMemberExpr,
                         target: MashValue,
                         thisTarget: Boolean,
                         invokeNullaryWhenVectorising: Boolean)(implicit context: EvaluationContext): MemberExprEvalResult = {
    val name = memberExpr.name
    val isSafe = memberExpr.isSafe
    val locationOpt = getLocation(memberExpr)
    evaluateMember(target, name, isSafe, invokeNullaryWhenVectorising, thisTarget, locationOpt)
  }

  def evaluateMember(target: MashValue,
                     name: String,
                     isSafe: Boolean,
                     invokeNullaryWhenVectorising: Boolean,
                     thisTarget: Boolean,
                     locationOpt: Option[SourceLocation]): MemberExprEvalResult = {
    val scalarLookup = MemberEvaluator.maybeLookupByString(target, name, includePrivate = thisTarget).map(result ⇒
      MemberExprEvalResult(result))
    def vectorisedLookup = vectorisedMemberLookup(target, name, isSafe, invokeNullaryWhenVectorising, locationOpt)
      .map(result ⇒ MemberExprEvalResult(result, wasVectorised = true))
    scalarLookup orElse
      vectorisedLookup getOrElse {
      if (isSafe)
        MemberExprEvalResult(MashNull)
      else
        throwCannotFindMemberException(target, name, locationOpt)
    }
  }

  def throwCannotFindMemberException(target: MashValue, name: String, locationOpt: Option[SourceLocation]): Nothing = {
    val names = getMemberNames(target)
    val message = s"Cannot find member '$name' in value of type ${target.typeName}${Suggestor.suggestionSuffix(names, name)}"
    throw new EvaluatorException(message, locationOpt)
  }

  private def throwCannotFindMemberException(target: MashValue, field: MashValue, locationOpt: Option[SourceLocation]): Nothing =
    field match {
      case s: MashString ⇒
        throwCannotFindMemberException(target, s, locationOpt)
      case _             ⇒
        val name = ToStringifier.safeStringify(field)
        val message = s"Cannot find member '$name' in value of type ${target.typeName}"
        throw new EvaluatorException(message, locationOpt)
    }

  def getLocation(memberExpr: AbstractMemberExpr): Option[SourceLocation] =
    memberExpr.sourceInfoOpt.flatMap(info ⇒ condOpt(info.node) {
      case ConcreteSyntax.MemberExpr(_, _, nameToken) ⇒
        SourceLocation(info.provenance, PointedRegion(nameToken.offset, nameToken.region))
    })

  private def vectorisedMemberLookup(target: MashValue,
                                     name: String,
                                     isSafe: Boolean,
                                     immediatelyResolveNullaryWhenVectorising: Boolean,
                                     locationOpt: Option[SourceLocation]): Option[MashList] =
    target match {
      case xs: MashList ⇒
        val options = xs.elements.map { element ⇒
          val memberOpt = MemberEvaluator.maybeLookupByString(element, name)
          val resolvedOpt =
            if (immediatelyResolveNullaryWhenVectorising)
              memberOpt.map(lookup ⇒ Evaluator.invokeNullaryFunctions(lookup, locationOpt))
            else
              memberOpt
          if (isSafe) resolvedOpt orElse Some(MashNull) else resolvedOpt
        }
        Utils.sequence(options).map(MashList(_))
      case _            ⇒
        None
    }

  private def maybeLookupInClass(target: MashValue,
                                 klass: MashClass,
                                 name: String,
                                 includePrivate: Boolean = false,
                                 includeShyMembers: Boolean = true): Option[BoundMethod] = {
    val directResultOpt =
      for {
        method ← klass.getMethod(name)
        if method.isPublic || includePrivate
      } yield BoundMethod(target, method, klass)
    def parentResultOpt = klass.parentOpt
      .flatMap(parentClass ⇒ maybeLookupInClass(target, parentClass, name))
    (directResultOpt orElse parentResultOpt).filter(includeShyMembers || !_.method.isShy)
  }

  def lookupByString(target: MashValue,
                     name: String,
                     includePrivate: Boolean = false,
                     locationOpt: Option[SourceLocation] = None): MashValue =
    maybeLookupByString(target, name, includePrivate).getOrElse(
      throwCannotFindMemberException(target, name, locationOpt))

  def lookup(target: MashValue,
             name: MashValue,
             includePrivate: Boolean = false,
             locationOpt: Option[SourceLocation] = None): MashValue =
    maybeLookup(target, name, includePrivate).getOrElse(
      throwCannotFindMemberException(target, name, locationOpt))

  def maybeLookup(target: MashValue,
                  name: MashValue,
                  includePrivate: Boolean = false,
                  includeShyMembers: Boolean = true): Option[MashValue] = name match {
    case s: MashString ⇒ maybeLookupByString(target, s.s, includePrivate, includeShyMembers)
    case _             ⇒ target match {
      case obj: MashObject ⇒ obj.get(name)
    }
  }

  /**
    * @return a bound method, a static method, or a field value corresponding to the given name in the target
    */
  def maybeLookupByString(target: MashValue,
                          name: String,
                          includePrivate: Boolean = false,
                          includeShyMembers: Boolean = true): Option[MashValue] =
    target match {
      case MashNumber(n, tagClassOpt)     ⇒ maybeLookupInClass(target, NumberClass, name) orElse tagClassOpt.flatMap(maybeLookupInClass(target, _, name))
      case MashString(s, tagClassOpt)     ⇒ maybeLookupInClass(target, StringClass, name) orElse tagClassOpt.flatMap(maybeLookupInClass(target, _, name))
      case MashNull                       ⇒ maybeLookupInClass(target, NullClass, name)
      case MashUnit                       ⇒ maybeLookupInClass(target, UnitClass, name)
      case b: MashBoolean                 ⇒ maybeLookupInClass(b, BooleanClass, name)
      case xs: MashList                   ⇒ maybeLookupInClass(xs, ListClass, name)
      case f: MashFunction                ⇒ maybeLookupInClass(f, FunctionClass, name)
      case bm: BoundMethod                ⇒ maybeLookupInClass(bm, BoundMethodClass, name)
      case klass: MashClass               ⇒ klass.getStaticMethod(name) orElse maybeLookupInClass(klass, ClassClass, name)
      case dt@MashWrapped(_: Instant)     ⇒ maybeLookupInClass(dt, DateTimeClass, name)
      case date@MashWrapped(_: LocalDate) ⇒ maybeLookupInClass(date, DateClass, name)
      case obj: MashObject                ⇒ obj.get(name) orElse maybeLookupInClass(obj, obj.classOpt getOrElse ObjectClass, name, includePrivate, includeShyMembers)
    }

  def getMemberNames(target: MashValue, includePrivate: Boolean = false): Seq[String] = {
    val memberNames = target match {
      case MashNumber(n, tagClassOpt)     ⇒ NumberClass.memberNames(includePrivate) ++ tagClassOpt.toSeq.flatMap(_.memberNames(includePrivate))
      case MashString(s, tagClassOpt)     ⇒ StringClass.memberNames(includePrivate) ++ tagClassOpt.toSeq.flatMap(_.memberNames(includePrivate))
      case MashNull                       ⇒ NullClass.memberNames(includePrivate)
      case MashUnit                       ⇒ UnitClass.memberNames(includePrivate)
      case b: MashBoolean                 ⇒ BooleanClass.memberNames(includePrivate)
      case xs: MashList                   ⇒ ListClass.memberNames(includePrivate)
      case f: MashFunction                ⇒ FunctionClass.memberNames(includePrivate)
      case bm: BoundMethod                ⇒ BoundMethodClass.memberNames(includePrivate)
      case klass: MashClass               ⇒ ClassClass.memberNames(includePrivate)
      case dt@MashWrapped(_: Instant)     ⇒ DateTimeClass.memberNames(includePrivate)
      case date@MashWrapped(_: LocalDate) ⇒ DateClass.memberNames(includePrivate)
      case obj: MashObject                ⇒
        obj.immutableFields.keys.toSeq.collect { case s: MashString ⇒ s.s } ++ (obj.classOpt getOrElse ObjectClass).memberNames(includePrivate)
    }
    (memberNames ++ AnyClass.memberNames(includePrivate)).distinct
  }

}