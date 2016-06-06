package com.github.mdr.mash

import com.github.mdr.mash.runtime.MashObject
import scala.collection.mutable.LinkedHashMap

case class ConfigOption(name: String, defaultValue: Any) {
  
  def path: Seq[String] = name.split("\\.").toSeq
  
  override def toString = name
  
}

object Config {

  object Language {
    val BareWords = ConfigOption("language.bareWords", defaultValue = false)
  }

  object Cli {
    val ShowStartupTips = ConfigOption("cli.showStartupTips", defaultValue = true)
  }

  val AllKeys = Seq(Language.BareWords, Cli.ShowStartupTips)

 def getConfig(configOpt: Option[MashObject], configOption: ConfigOption): Any = {
    val valueOpt = for {
      config <- configOpt
      value <- getConfig(config, configOption.path)
    } yield value
    valueOpt.getOrElse(configOption.defaultValue)
  }

  private def getConfig(mo: MashObject, path: Seq[String]): Option[Any] = {
    for {
      first ← path.headOption
      rest = path.tail
      firstValue ← mo.getField(first)
      restValue ← rest match {
        case Seq() ⇒
          Some(firstValue)
        case _ ⇒
          firstValue match {
            case obj: MashObject ⇒ getConfig(obj, rest)
            case _               ⇒ None
          }
      }
    } yield restValue
  }

  
 def defaultConfig = {
    val config = MashObject(LinkedHashMap())
    def addConfigOption(obj: MashObject, path: Seq[String], value: Any) {
      path match {
        case Seq()    ⇒
        case Seq(key) ⇒ obj.set(key, value)
        case Seq(head, rest @ _*) ⇒
          val childObj = obj.getField(head) match {
            case Some(obj: MashObject) ⇒
              obj
            case _ ⇒
              val newObj = MashObject(LinkedHashMap())
              obj.set(head, newObj)
              newObj
          }
          addConfigOption(childObj, rest, value)
      }
    }
    for (configOption ← Config.AllKeys) {
      addConfigOption(config, configOption.path, configOption.defaultValue)
    }
    config
  }

}