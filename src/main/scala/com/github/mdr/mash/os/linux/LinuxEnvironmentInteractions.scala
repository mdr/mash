package com.github.mdr.mash.os.linux

import com.github.mdr.mash.os.EnvironmentInteractions
import java.nio.file.Paths

object LinuxEnvironmentInteractions extends EnvironmentInteractions {

  override def home = Paths.get(System.getProperty("user.home"))

}