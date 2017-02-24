package com.github.mdr.mash.ns.git.remote

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, FullyQualifiedName, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString }
import org.eclipse.jgit.transport.RemoteConfig

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object ListFunction extends MashFunction("git.remote.list") {

  override def aliases = Seq(FullyQualifiedName("git.remotes"))

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashList = {
    GitHelper.withGit { git ⇒
      val remoteConfigs = git.remoteList().call().asScala
      MashList(remoteConfigs.map(asMashObject))
    }
  }

  def asMashObject(config: RemoteConfig): MashObject = {
    val name = config.getName
    val fetchUrls = config.getURIs.asScala.map(url ⇒ MashString(url.toString))
    val pushUrls = config.getPushURIs.asScala.map(url ⇒ MashString(url.toString))
    import RemoteClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(name, RemoteNameClass),
        FetchUrls -> MashList(fetchUrls),
        PushUrls -> MashList(pushUrls)),
      RemoteClass)
  }

  override def typeInferenceStrategy = Seq(RemoteClass)

  override def summaryOpt = Some("Get a list of Git remotes")
}

