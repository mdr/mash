package com.github.mdr.mash.language

import java.time.Instant
import java.util.IdentityHashMap

import com.github.mdr.mash.evaluator.ToStringifier.stringify
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.parser.StringEscapes.escapeChars
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.Utils.sequence

import scala.PartialFunction.cond

object ValueToExpression {

  def getExpression(value: MashValue): Option[String] = new ValueToExpressionContext().getExpression(value)
}

private[language] class ValueToExpressionContext {

  private val visitingMap: IdentityHashMap[MashValue, Boolean] = new IdentityHashMap

  def getExpression(value: MashValue): Option[String] =
    if (visitingMap containsKey value)
      None
    else {
      visitingMap.put(value, true)
      try
        actualGetExpression(value)
      finally
        visitingMap.remove(value)
    }

  private def actualGetExpression(value: MashValue): Option[String] = value match {
    case MashNull | _: MashBoolean | _: MashNumber ⇒ Some(stringify(value))
    case s: MashString                             ⇒ Some(s""""${escapeChars(s.s)}"""")
    case xs: MashList                              ⇒ getExpression(xs)
    case obj: MashObject                           ⇒ getExpression(obj)
    case MashWrapped(instant: Instant)             ⇒ Some(s"time.fromMillisSinceEpoch ${instant.toEpochMilli}")
    case _                                         ⇒ None
  }

  private def getExpression(xs: MashList): Option[String] =
    sequence(xs.immutableElements.map(getExpression)).map(_.mkString("[", ", ", "]"))

  private def getExpression(obj: MashObject): Option[String] = {
    val fieldAndValueExpressionsOpt = sequence(
      for ((field, value) ← obj.immutableFields.toSeq) yield
        for {
          fieldExpression ← getExpression(field)
          valueExpression ← getExpression(value)
        } yield FieldAndValueExpression(fieldExpression, valueExpression, field))
    fieldAndValueExpressionsOpt.map(assembleObjectExpression)
  }

  private def assembleObjectExpression(fieldAndValueExpressions: Seq[FieldAndValueExpression]): String =
    fieldAndValueExpressions.map(_.expression).mkString("{ ", ", ", " }")

  private case class FieldAndValueExpression(fieldExpression: String, valueExpression: String, field: MashValue) {

    private def fieldRequiresParen = !cond(field) {
      case MashNull | _: MashBoolean | _: MashNumber | _: MashString ⇒ true
    }

    private def fieldCanBeBare = cond(field) {
      case s: MashString ⇒ MashLexer.isLegalIdentifier(s.s)
    }

    private def finalFieldExpression =
      if (fieldRequiresParen)
        s"($fieldExpression)"
      else if (fieldCanBeBare)
        field.toString
      else
        fieldExpression

    def expression = s"$finalFieldExpression: $valueExpression"

  }

}
