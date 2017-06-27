package com.github.mdr.mash.completions

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.{ Object, UserClass }
import com.github.mdr.mash.ns.collections.ListClass
import com.github.mdr.mash.ns.core.{ BoundMethodClass, ClassClass, FunctionClass, ObjectClass }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }
import com.github.mdr.mash.utils.Utils

import scala.PartialFunction._

object MemberFinder {

  def getMembers(targetType: Type, canVectorise: Boolean = true): Seq[MemberInfo] =
    targetType match {
      case Type.Instance(klass)              ⇒ getMembers(klass)
      case Type.UserClassInstance(userClass) ⇒ getMembers(userClass)
      case userClass: Type.UserClass         ⇒ getStaticMembers(userClass)
      case Type.Tagged(baseClass, tagClass)  ⇒ distinct(getMembers(tagClass) ++ getMembers(baseClass))
      case Type.Generic(klass, _*)           ⇒ getMembers(klass)
      case Type.BuiltinFunction(_)           ⇒ getMembers(FunctionClass)
      case Type.BoundBuiltinMethod(_, _)     ⇒ getMembers(BoundMethodClass)
      case objectType: Type.Object           ⇒ getObjectMembers(objectType)
      case Type.Seq(elementType)             ⇒ getListMembers(elementType, canVectorise)
      case _                                 ⇒ Seq()
    }

  private def getObjectMembers(objectType: Object): Seq[MemberInfo] = {
    val fieldMembers = objectType.knownFields.keys.toSeq.map(f ⇒ MemberInfo(f, isField = true))
    distinct(fieldMembers ++ getMembers(ObjectClass))
  }

  private def getListMembers(elementType: Type, canVectorise: Boolean): Seq[MemberInfo] = {
    val seqMembers = getMembers(ListClass)
    if (canVectorise) {
      val elementMembers = getMembers(elementType, canVectorise = false).map(_.copy(isVectorised = true))
      distinct(seqMembers ++ elementMembers)
    } else
      seqMembers
  }

  private def getMembers(klass: MashClass): Seq[MemberInfo] = {
    val fieldMembers = klass.fields.map(f ⇒
      MemberInfo(f.name, classNameOpt = klass.nameOpt, descriptionOpt = f.summaryOpt, isField = true))
    val methodMembers = klass.methods.flatMap(m ⇒
      m.names.map(name ⇒ MemberInfo(name, classNameOpt = klass.nameOpt, descriptionOpt = m.summaryOpt, isField = false)))
    val parentClassMembers = klass.parentOpt.toSeq.flatMap(parentClass ⇒ getMembers(parentClass))
    distinct(fieldMembers ++ methodMembers ++ parentClassMembers)
  }

  private def getMembers(userClass: UserClass): Seq[MemberInfo] = {
    val fieldMembers = userClass.params.params.flatMap(_.nameOpt).map(name ⇒
      MemberInfo(name, classNameOpt = Some(userClass.name), isField = true))
    val methodMembers = userClass.methods.collect { case (name, method) if method.isPublic ⇒
      MemberInfo(name, classNameOpt = Some(userClass.name), isField = false)
    }
    val parentClassMembers = getMembers(ObjectClass)
    distinct(fieldMembers ++ methodMembers ++ parentClassMembers)
  }

  private def getStaticMembers(userClass: UserClass): Seq[MemberInfo] = {
    val staticMethodMembers = Seq(MemberInfo("new", classNameOpt = Some(userClass.name), isField = false, isStatic = true))
    distinct(staticMethodMembers ++ getMembers(ClassClass))
  }

  def getValueMembers(value: MashValue): Option[Seq[MemberInfo]] =
    condOpt(value) {
      case klass: MashClass ⇒ getValueMembers(klass)
      case obj: MashObject  ⇒ getValueMembers(obj)
    }

  private def getValueMembers(klass: MashClass): Seq[MemberInfo] = {
    val staticMethodMembers = klass.staticMethods.map(method ⇒
      MemberInfo(method.name, classNameOpt = klass.nameOpt, descriptionOpt = method.summaryOpt, isField = false, isStatic = true))
    distinct(staticMethodMembers ++ getMembers(ClassClass))
  }

  private def getValueMembers(obj: MashObject): Seq[MemberInfo] =
    obj.classOpt match {
      case Some(klass) ⇒
        val extraFieldMembers = obj.immutableFields.keys.toSeq.collect { case s: MashString ⇒ s.s }.filterNot(klass.fieldsMap.contains).map(MemberInfo(_, isField = true))
        distinct(extraFieldMembers ++ getMembers(klass))
      case None        ⇒
        val fieldMembers = obj.immutableFields.keys.toSeq.collect { case s: MashString ⇒ s.s }.map(MemberInfo(_, isField = true))
        distinct(fieldMembers ++ getMembers(ObjectClass))
    }

  private def distinct(members: Seq[MemberInfo]) = Utils.distinctBy[MemberInfo, String](members, _.name)

}