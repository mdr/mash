package com.github.mdr.mash.assist

import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass

sealed trait Assistable

object Assistable {

  case class Function(f: MashFunction) extends Assistable

  case class FunctionType(f: Type.UserDefinedFunction) extends Assistable

  case class Method(method: MashMethod) extends Assistable

  case class MethodType(method: Type.UserDefinedFunction) extends Assistable

  case class ConstructorType(userClass: UserClass) extends Assistable

}

case class AssistanceState(title: String, lines: Seq[String])
