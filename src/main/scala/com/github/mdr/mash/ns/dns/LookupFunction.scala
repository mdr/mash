package com.github.mdr.mash.ns.dns

import java.net.InetAddress

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.net.HostClass.IpAddressesMethod
import com.github.mdr.mash.runtime.MashList

object LookupFunction extends MashFunction("dns.lookup") {

  object Params {
    val Host = Parameter(
      nameOpt = Some("host"),
      summary = "Host name")
  }
  import Params._

  val params = ParameterModel(Seq(Host))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val host = boundParams.validateString(Host).s
    val addresses = InetAddress.getAllByName(host).map(IpAddressesMethod.asMashString)
    MashList(addresses)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(StringClass))

  override def summary = "Look up the IP addresses of the given host"

}