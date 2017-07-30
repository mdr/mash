package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.classes.{ BoundMethod, Field, MashClass }
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter }
import com.github.mdr.mash.runtime._

object HelpCreator {

  def getHelp(item: MashValue): MashValue = item match {
    case f: MashFunction  ⇒ f
    case bm: BoundMethod  ⇒ getMethodHelp(bm)
    case klass: MashClass ⇒ klass
    case value            ⇒ value.primaryClass
  }

  private def getMethodHelp(boundMethod: BoundMethod): MashObject =
    getMethodHelp(boundMethod.method, boundMethod.klass)

  private def getMethodHelp(m: MashMethod, klass: MashClass): MashObject =
    MethodHelpClass.create(m.name, klass)

  def getFieldHelp(field: Field, klass: MashClass): MashObject =
    FieldHelpClass.create(name = field.name, klass = klass)

}