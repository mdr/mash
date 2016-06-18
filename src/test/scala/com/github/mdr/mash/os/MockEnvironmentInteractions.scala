package com.github.mdr.mash.os

import java.nio.file.Path
import java.nio.file.Paths

object MockEnvironmentInteractions {

  def apply(home: String): MockEnvironmentInteractions = MockEnvironmentInteractions(Paths.get(home))

}

case class MockEnvironmentInteractions(home: Path = Paths.get("/")) extends EnvironmentInteractions