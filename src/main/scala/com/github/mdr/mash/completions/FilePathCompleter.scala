package com.github.mdr.mash.completions

import java.io.IOException
import java.nio.file.Paths

import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.FileSystem

class FilePathCompleter(fileSystem: FileSystem) {

  def getCompletions(s: String, directoriesOnly: Boolean = false): Seq[String] =
    if (s == "..")
      Seq("..")
    else {
      val path = Paths.get(s)
      val searchPath =
        if (s endsWith "/")
          path
        else
          Option(path.getParent).getOrElse(Paths.get(""))
      val prefix =
        if (s endsWith "/")
          ""
        else
          path.getFileName.toString
      val ignoreDotFiles = !prefix.startsWith(".")
      try {
        fileSystem
          .getChildren(searchPath, ignoreDotFiles = ignoreDotFiles, recursive = false)
          .filter(_.fileType == FileTypeClass.Values.Dir || !directoriesOnly)
          .filter(_.path.getFileName.toString startsWith prefix)
          .map { f ⇒
            val suffix = if (f.fileType == FileTypeClass.Values.Dir) "/" else ""
            searchPath.resolve(f.path.getFileName).toString + suffix
          }.sorted
      } catch {
        case _: IOException ⇒
          Seq()
      }
    }

}