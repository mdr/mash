package com.github.mdr.mash

import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashString, MashValue }
import com.github.mdr.mash.view.render.DiscoMode

import scala.annotation.tailrec

case class ConfigOption(name: String, defaultValue: MashValue) {

  def path: Seq[String] = name.split("\\.").toSeq

  override def toString = name

}

object ConfigWrapper {

  def fromGlobals(globalVariables: MashObject): ConfigWrapper =
    ConfigWrapper(globalVariables.get(StandardEnvironment.Config).flatMap(_.asObject))

}

case class ConfigWrapper(configObjectOpt: Option[MashObject]) {

  def bareWords: Boolean = getBooleanConfig(Config.Language.BareWords)

  def showStartupTips: Boolean = getBooleanConfig(Config.Cli.ShowStartupTips)

  def viewFuzzyTime: Boolean = getBooleanConfig(Config.View.FuzzyTime)

  def browseLargeOutput: Boolean = getBooleanConfig(Config.View.BrowseLargeOutput)

  def discoModeOpt: Option[DiscoMode] = getConfig(Config.View.DiscoBorders) match {
    case MashString("animated", _) ⇒ Some(DiscoMode.Animated)
    case value if value.isTruthy   ⇒ Some(DiscoMode.Static)
    case _                         ⇒ None
  }

  private def getBooleanConfig(configOption: ConfigOption): Boolean =
    getConfig(configOption).isTruthy

  def getConfig(configOption: ConfigOption): MashValue = {
    val valueOpt =
      for {
        configObject ← configObjectOpt
        value ← getConfig(configObject, configOption.path)
      } yield value
    valueOpt.getOrElse(configOption.defaultValue)
  }

  private def getConfig(obj: MashObject, path: Seq[String]): Option[MashValue] =
    for {
      first ← path.headOption
      rest = path.tail
      firstValue ← obj.get(first)
      restValue ← rest match {
        case Seq() ⇒
          Some(firstValue)
        case _     ⇒
          firstValue match {
            case obj: MashObject ⇒ getConfig(obj, rest)
            case _               ⇒ None
          }
      }
    } yield restValue

}

object Config {

  object Language {
    val BareWords = ConfigOption("language.bareWords", defaultValue = MashBoolean.False)
  }

  object Cli {
    val ShowStartupTips = ConfigOption("cli.showStartupTips", defaultValue = MashBoolean.True)
  }

  object View {
    val FuzzyTime = ConfigOption("view.fuzzyTime", defaultValue = MashBoolean.True)
    val BrowseLargeOutput = ConfigOption("view.browseLargeOutput", defaultValue = MashBoolean.True)
    val DiscoBorders = ConfigOption("view.discoBorders", defaultValue = MashBoolean.False)
  }

  val AllKeys = Seq(
    Cli.ShowStartupTips,
    Language.BareWords,
    View.BrowseLargeOutput,
    View.DiscoBorders,
    View.FuzzyTime)

  def defaultConfig: MashObject = {
    val config = MashObject.empty

    @tailrec
    def addConfigOption(obj: MashObject, path: Seq[String], value: MashValue) {
      path match {
        case Seq()              ⇒
        case Seq(key)           ⇒ obj.set(key, value)
        case Seq(head, rest@_*) ⇒
          val childObj = obj.get(head) match {
            case Some(obj: MashObject) ⇒
              obj
            case _                     ⇒
              val newObj = MashObject.empty
              obj.set(head, newObj)
              newObj
          }
          addConfigOption(childObj, rest, value)
      }
    }

    for (configOption ← Config.AllKeys)
      addConfigOption(config, configOption.path, configOption.defaultValue)
    config
  }

}