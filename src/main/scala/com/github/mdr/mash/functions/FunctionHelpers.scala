package com.github.mdr.mash.functions

import java.nio.file.Path
import java.nio.file.Paths

import scala.PartialFunction._

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.runtime._

object FunctionHelpers {

  def interpretAsPaths(x: MashValue): Seq[Path] =
    x match {
      case xs: MashList ⇒ xs.items.flatMap(interpretAsPaths)
      case _            ⇒ Seq(interpretAsPath(x))
    }

  def safeInterpretAsPath(x: MashValue): Option[Path] =
    condOpt(x) {
      case MashString(s, _) ⇒
        Paths.get(s)
      case mo: MashObject if mo.classOpt == Some(PathSummaryClass) ⇒
        Paths.get(MemberEvaluator.lookup(mo, PathSummaryClass.Fields.Path.name).asInstanceOf[MashString].s)
    }

  def interpretAsPath(x: MashValue): Path =
    safeInterpretAsPath(x).getOrElse(throw new EvaluatorException("Could not interpret as path: " + x))

  def asPathString(p: Path) = MashString(p.toString, PathClass)

  def asPathString(s: String) = MashString(s, PathClass)

  def interpretAsFunction(f: MashValue): (MashValue ⇒ MashValue) = o ⇒ {
    val args = Arguments(Seq(EvaluatedArgument.PositionArg(o, None)))
    InvocationEvaluator.callFunction(f, args)
  }

  def interpretAsSequence(x: MashValue): Seq[MashValue] = x match {
    case xs: MashList          ⇒ xs.items
    case MashString(s, tagOpt) ⇒ s.toSeq.map(c ⇒ MashString(c.toString, tagOpt))
    case _                     ⇒ throw new EvaluatorException("Could not interpret as sequence: " + x)
  }

}