package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.{ MashNumber, MashObject, MashValue }

object UidClass extends MashClass("os.Uid") {

  private val userInteractions = UserInteractions.default

  override lazy val methods = {
    val liftedFields = UserSummaryClass.fields.map(liftUserSummaryField)
    val liftedMethods = UserSummaryClass.methods.map(liftUserSummaryMethod)
    (InfoMethod +: liftedFields) ++ liftedMethods
  }

  private def liftUserSummaryField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      userSummary.fields(field.name)
    }

    override def typeInferenceStrategy =
      ConstantMethodTypeInferenceStrategy(UserSummaryClass.fieldsMap(field.name).fieldType)

    override def summaryOpt = Some(s"Access '${field.name}' property of this user")

    override def descriptionOpt = field.descriptionOpt

  }

  private def liftUserSummaryMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      method.apply(userSummary, arguments)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summaryOpt = method.summaryOpt

    override def descriptionOpt = method.descriptionOpt

  }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      UserSummaryClass.fromPasswdEntry(passwdEntry)
    }

    override def typeInferenceStrategy = UserSummaryClass

    override def summaryOpt = Some("Fetch information about the user with this uid")

  }

  override def summaryOpt = Some("Tag class for user ID (UID)")

  override def parentOpt = Some(AnyClass)

}