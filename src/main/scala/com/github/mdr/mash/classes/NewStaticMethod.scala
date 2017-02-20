package com.github.mdr.mash.classes

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

case class NewStaticMethod(klass: MashClass) extends MashFunction(MashClass.ConstructorMethodName) {

  override def params: ParameterModel = {
    val params =
      for (field ← klass.fields) yield
        Parameter(
          nameOpt = Some(field.name),
          summaryOpt = field.summaryOpt)
    ParameterModel(params)
  }

  override def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val fields =
      for {
        param <- params.params
        boundName ← param.boundNames
      } yield boundName -> boundParams(boundName)
    MashObject.of(fields, klass)
  }

  override def summaryOpt: Option[String] = Some(s"Construct a new ${klass.name} object")

}