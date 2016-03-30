package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxUserInteractions

object UidClass extends MashClass("os.Uid") {

  private val userInteractions = LinuxUserInteractions

  override lazy val methods = {
    val liftedFields = UserSummaryClass.fields.map(liftUserSummaryField)
    val liftedMethods = UserSummaryClass.methods.map(liftUserSummaryMethod)
    (InfoMethod +: liftedFields) ++ liftedMethods
  }

  private def liftUserSummaryField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Any = {
      params.validate(arguments)
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      userSummary.fields(field.name)
    }

    override def typeInferenceStrategy = 
      ConstantMethodTypeInferenceStrategy(UserSummaryClass.fieldsMap(field.name).fieldType)

    override def summary = s"Access '${field.name}' property of this user"

    override def descriptionOpt = field.descriptionOpt

  }

  private def liftUserSummaryMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    def apply(target: Any, arguments: Arguments): Any = {
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      method.apply(userSummary, arguments)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summary = method.summary

    override def descriptionOpt = method.descriptionOpt

  }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      UserSummaryClass.fromPasswdEntry(passwdEntry)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(UserSummaryClass))

    override def summary = "Fetch information about the user with this uid"

  }

  override def summary = "Tag class for user ID (UID)"
}