package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, Field, MashClass }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.parser.DocComment
import com.github.mdr.mash.runtime.MashObject

case class UserDefinedClass(docCommentOpt: Option[DocComment],
                            override val name: String,
                            params: ParameterModel,
                            override val methods: Seq[UserDefinedMethod]) extends MashClass(nameOpt = Some(name)) {

  override val fields = params.params.map { param ⇒
    val fieldName = param.nameOpt.getOrElse("anon")
    Field(fieldName, param.descriptionOpt, AnyClass)
  }

  override val staticMethods = Seq(
    NewStaticMethod)

  object NewStaticMethod extends MashFunction(MashClass.ConstructorMethodName) {
    override def apply(arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val fields =
        for {
          param <- params.params
          boundName ← param.boundNames
        } yield boundName -> boundParams(boundName)
      MashObject.of(fields, UserDefinedClass.this)
    }

    override def summaryOpt = Some(s"Construct a new $name")

    override def params: ParameterModel = UserDefinedClass.this.params

    override def typeInferenceStrategy = UserDefinedClass.this

  }

  override def summaryOpt = docCommentOpt.map(_.summary)

  override def descriptionOpt = docCommentOpt.flatMap(_.descriptionOpt)

  // Don't want to accidentally compare equal to another class
  override def equals(that: Any) = PartialFunction.cond(that) { case that: AnyRef ⇒ this eq that }

  override def hashCode = System.identityHashCode(this)

}
