package com.github.mdr.mash

import java.io.File
import java.nio.file.{ Files, Path }

object Mash {

  val MashDir: Path = new File(System.getProperty("user.home"), ".mash").toPath

  def ensureMashDirExists(): Path = {
    if (!Files.exists(MashDir))
      Files.createDirectory(MashDir)
    MashDir
  }

}