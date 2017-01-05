package com.github.mdr.mash.ns.net

import java.net.InetAddress

import com.github.mdr.mash.evaluator.{ Arguments, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.ns.core.{ AnyClass, StringClass }
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

object HostClass extends MashClass("net.Host") {

  override val methods = Seq(IpAddressesMethod)

  override def summary: String = "Host"

  object IpAddressesMethod extends MashMethod("ipAddresses") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val host = target.asInstanceOf[MashString].s
      val addresses = InetAddress.getAllByName(host).map(asMashString)
      MashList(addresses)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(StringClass))

    override def summary = "Look up the IP addresses of the given host"

    def asMashString(address: InetAddress) = MashString(address.getHostAddress)

  }

  override def parentOpt = Some(AnyClass)

}

