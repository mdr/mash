package com.github.mdr.mash.os.linux

import java.nio.file.Paths

import com.github.mdr.mash.os.EnvironmentInteractions

object LinuxEnvironmentInteractions extends EnvironmentInteractions {

  override def home = Paths.get(System.getProperty("user.home"))

}