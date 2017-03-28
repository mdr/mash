package com.github.mdr.mash.ns.http

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashValue

object HeaderClass extends MashClass("http.Header") {

  object Fields {
    lazy val Name = Field("name", Some("Header field name"), StringClass)
    lazy val Value = Field("value", Some("Header field value"), StringClass)
  }

  import Fields._

  override val fields = Seq(Name, Value)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(obj: MashValue) extends AbstractObjectWrapper(obj) {
    def name = getStringField(Name)
    def value = getStringField(Value)
  }

  def asHeader(obj: MashValue): Header = {
    val wrapper = Wrapper(obj)
    Header(wrapper.name, wrapper.value)
  }

  override def summaryOpt = Some("An HTTP header")
}