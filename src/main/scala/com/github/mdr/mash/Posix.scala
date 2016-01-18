package com.github.mdr.mash

import jnr.posix.POSIXFactory

object Posix {

  lazy val posix = POSIXFactory.getPOSIX()

}