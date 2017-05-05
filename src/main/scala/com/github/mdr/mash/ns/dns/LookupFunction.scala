package com.github.mdr.mash.ns.dns

import java.net.InetAddress

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.net.HostClass.IpAddressesMethod
import com.github.mdr.mash.runtime.MashList

object LookupFunction extends MashFunction("dns.lookup") {

  object Params {
    val Host = Parameter(
      nameOpt = Some("host"),
      summaryOpt = Some("Host name"))
  }
  import Params._

  val params = ParameterModel(Seq(Host))

  def call(boundParams: BoundParams): MashList = {
    val host = boundParams.validateString(Host).s
    val addresses = InetAddress.getAllByName(host).map(IpAddressesMethod.asMashString)
    MashList(addresses)
  }

  override def typeInferenceStrategy = Seq(StringClass)

  override def summaryOpt = Some("Look up the IP addresses of the given host")

}