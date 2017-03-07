package com.github.mdr.mash

import java.nio.file.Path

class WorkingDirectoryStack(initial: Path) {

  private var dirs: Seq[Path] = Seq(initial)
  private var pos: Int = 0

  def push(path: Path) {
    dirs = dirs :+ path
    pos = dirs.length - 1
  }

  def oldDirs: Seq[Path] = dirs take pos

  def back(): Option[Path] =
    if (pos > 0) {
      pos -= 1
      Some(dirs(pos))
    } else
      None

  def forward(): Option[Path] =
    if (pos < dirs.length - 1) {
      pos += 1
      Some(dirs(pos))
    } else
      None

}