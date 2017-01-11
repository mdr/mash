package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, Field, MashClass }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashObject

case class UserDefinedClass(override val name: String,
                            params: ParameterModel,
                            override val methods: Seq[UserDefinedMethod]) extends MashClass(nameOpt = Some(name)) {

  override val fields = params.params.map { param ⇒
    val fieldName = param.nameOpt.getOrElse("anon")
    Field(fieldName, s"Field '$fieldName'", AnyClass)
  }

  override val staticMethods = Seq(
    NewStaticMethod)

  object NewStaticMethod extends MashFunction(MashClass.ConstructorMethodName) {
    override def apply(arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val fields =
        for (param <- params.params)
          yield param.nameOpt.getOrElse("anon") -> boundParams(param)
      MashObject.of(fields, UserDefinedClass.this)
    }

    override def summary: String = s"Construct a new $name"

    override def params: ParameterModel = UserDefinedClass.this.params

    override def typeInferenceStrategy = UserDefinedClass.this

  }

  override def summary: String = s"Class $name"

}
