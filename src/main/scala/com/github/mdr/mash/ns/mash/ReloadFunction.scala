package com.github.mdr.mash.ns.mash

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.runtime._

object ReloadFunction extends MashFunction("mash.reload") {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashValue = {
    val freshGlobals = StandardEnvironment.createGlobalVariables
    for ((name, value) ← freshGlobals.immutableFields)
      Singletons.globals.set(name, value)
    Singletons.globals.set(StandardEnvironment.Ns, Singletons.ns)
    Singletons.ns.clear()
    Singletons.loader.load()
    Singletons.initScriptRunner.processInitFile()
    MashUnit
  }

  override def summaryOpt = Some("Reload init.mash and library files")

  override def typeInferenceStrategy = Unit

}
