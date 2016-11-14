package com.github.mdr.mash.functions

import java.io.File
import java.nio.file.{ Path, Paths }

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.ns.os.{ PathClass, PathSummaryClass }
import com.github.mdr.mash.runtime._

import scala.PartialFunction._

object FunctionHelpers {

  def interpretAsPaths(value: MashValue): Seq[Path] =
    value match {
      case xs: MashList ⇒ xs.items.flatMap(interpretAsPaths)
      case _            ⇒ Seq(interpretAsPath(value))
    }

  def safeInterpretAsPath(x: MashValue): Option[Path] =
    condOpt(x) {
      case MashString(s, _) ⇒
        Paths.get(s)
      case obj: MashObject if obj.classOpt == Some(PathSummaryClass) ⇒
        Paths.get(obj(PathSummaryClass.Fields.Path).asInstanceOf[MashString].s)
    }

  def interpretAsPath(value: MashValue): Path =
    safeInterpretAsPath(value).getOrElse(
      throw new EvaluatorException(s"Could not an object of type ${value.typeName} as a Path"))

  def asPathString(p: Path) = MashString(p.toString, PathClass)

  def asPathString(s: String) = MashString(s, PathClass)

  def asPathString(f: File) = MashString(f.toString, PathClass)

  def interpretAsFunction(f: MashValue): (MashValue ⇒ MashValue) = { // TODO: share code with BoundParams
    def runFunction(value: MashValue) = {
      val arg = EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ value), None)
      InvocationEvaluator.callFunction(f, Arguments(Seq(arg)))
    }
    runFunction
  }

  def interpretAsSequence(value: MashValue): Seq[MashValue] = value match {
    case xs: MashList          ⇒ xs.items
    case MashString(s, tagOpt) ⇒ s.toSeq.map(c ⇒ MashString(c.toString, tagOpt))
    case _                     ⇒ throw new EvaluatorException(s"Could not interpret value of type ${value.typeName} as a sequence")
  }

}