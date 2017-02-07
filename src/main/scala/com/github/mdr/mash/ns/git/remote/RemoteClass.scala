package com.github.mdr.mash.ns.git.remote

import com.github.mdr.mash.evaluator.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass

object RemoteClass extends MashClass("git.remote.Remote") {

  object Fields {
    val Name = Field("name", Some("Name of the remote"), StringClass taggedWith RemoteNameClass)
    val FetchUrls = Field("fetchUrls", Some("URLs to fetch from"), Type.Seq(StringClass))
    val PushUrls = Field("pushUrls", Some("URLs to push to"), Type.Seq(StringClass))
  }

  import Fields._

  override lazy val fields = Seq(Name, FetchUrls, PushUrls)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("A Git remote")

}