package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator.AbstractToStringMethod
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashValue

object IdentityClass extends MashClass("git.Identity") {

  object Fields {
    val Name = Field("author", Some("Name"), StringClass)
    val Email = Field("email", Some("Email address"), StringClass)
  }
  import Fields._

  override lazy val fields = Seq(Name, Email)

  override lazy val methods = Seq(
    ToStringMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("A combination of a person identity and time in Git")

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {
    def name: String = getStringField(Name)
    def email: String = getStringField(Email)
  }

  object ToStringMethod extends AbstractToStringMethod {

    override def toString(target: MashValue) = {
      val identity = Wrapper(target)
      s"${identity.name} <${identity.email}>"
    }

  }

}