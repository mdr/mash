package com.github.mdr.mash

import java.nio.file.Path
import java.io.File
import java.nio.file.Files

object Mash {

  val MashDir: Path = new File(System.getProperty("user.home"), ".mash").toPath

  def ensureMashDirExists(): Path = {
    if (!Files.exists(MashDir))
      Files.createDirectory(MashDir)
    MashDir
  }

}