package com.github.mdr.mash

import java.nio.file.Path

class WorkingDirectoryStack {

  private var dirs: Seq[Path] = Seq()

  def push(path: Path) {
    dirs = dirs :+ path
  }

  def oldDirs: Seq[Path] = dirs

}