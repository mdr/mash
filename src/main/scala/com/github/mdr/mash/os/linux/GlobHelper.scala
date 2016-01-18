package com.github.mdr.mash.os.linux

import java.nio.file.Paths
import java.nio.file.Path
import scala.collection.JavaConverters._

object GlobHelper {

  private def makePath(segments: Seq[String], absolute: Boolean): Path = {
    val prefix = if (absolute) "/" else ""
    segments match {
      case Seq(first, rest @ _*) ⇒ Paths.get(prefix + first, rest: _*)
      case Seq()                 ⇒ Paths.get(prefix)
    }
  }

  def globStart(s: String): Path = {
    val path = Paths.get(s)
    val segments = path.iterator.asScala.toSeq
    def noGlobChars(segment: String) = """[$*{}\\]""".r.findFirstIn(segment).isEmpty
    val prefix = segments.map(_.toString).takeWhile(noGlobChars)
    makePath(prefix, absolute = path.isAbsolute)
  }

}