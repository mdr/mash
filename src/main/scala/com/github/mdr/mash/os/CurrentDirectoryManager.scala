package com.github.mdr.mash.os

import java.nio.file.Path

import com.github.mdr.mash.WorkingDirectoryStack

object CurrentDirectoryManager {

  sealed trait Result

  case object Success extends Result

  case object NotADirectory extends Result

}

case class CurrentDirectoryManager(fileSystem: FileSystem, workingDirectoryStack: WorkingDirectoryStack) {
  import CurrentDirectoryManager._

  def changeDirectory(path: Path): Result = {
    val result =
      if (fileSystem.isDirectory(path)) {
        fileSystem.chdir(path)
        Success
      } else
        NotADirectory
    workingDirectoryStack.push(fileSystem.pwd)
    result
  }

}
