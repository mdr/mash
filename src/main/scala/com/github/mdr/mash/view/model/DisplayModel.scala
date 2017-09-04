package com.github.mdr.mash.view.model

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help.{ FieldHelpClass, MethodHelpClass }
import com.github.mdr.mash.view.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.view.ViewConfig

trait DisplayModel

object DisplayModel {

  def getDisplayModel(value: MashValue, viewConfig: ViewConfig, terminalSize: Dimensions): DisplayModel =
    value match {
      case obj: MashObject if obj.classOpt == Some(MethodHelpClass) ⇒
        new HelpModelCreator(terminalSize, viewConfig).createForMethod(obj)
      case obj: MashObject if obj.classOpt == Some(FieldHelpClass)  ⇒
        new HelpModelCreator(terminalSize, viewConfig).createForField(obj)
      case klass: MashClass                                         ⇒
        new HelpModelCreator(terminalSize, viewConfig).createForClass(klass)
      case f: MashFunction                                          ⇒
        new HelpModelCreator(terminalSize, viewConfig).createForFunction(f)
      case _ if isSuitableForTwoDTable(value)                       ⇒
        new TwoDTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(value)
      case obj: MashObject if obj.nonEmpty                          ⇒
        new SingleObjectTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(obj)
      case xs: MashList                                             ⇒
        new TextLinesModelCreator(viewConfig).create(xs)
      case _                                                        ⇒
        new ValueModelCreator(viewConfig).create(value)
    }

}