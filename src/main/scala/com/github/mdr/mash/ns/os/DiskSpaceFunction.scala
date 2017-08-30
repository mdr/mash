package com.github.mdr.mash.ns.os

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.utils.StringUtils
import org.apache.commons.io.IOUtils

import scala.collection.immutable.ListMap

object DiskSpaceFunction extends MashFunction("os.diskSpace") {

  private val Regex = """^([^\s]+\s?[^\s]+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)%\s+[^/]*(.*?)\s*$""".r

  override def params = ParameterModel.Empty

  override def call(boundParams: BoundParams): MashList = {
    val lines = runDf()
    val contentLines = StringUtils.splitIntoLines(lines).init
    val entries = contentLines.collect {
      case Regex(filesystem, total, used, available, percent, mountpoint) ⇒
        create(filesystem, total.toLong * 1024, used.toLong * 1024, available.toLong * 1024, mountpoint)
    }
    MashList(entries)
  }

  private def create(filesystem: String, total: Long, used: Long, available: Long, mountpoint: String): MashObject = {
    import DiskSpaceEntryClass.Fields._
    MashObject.of(
      ListMap(
        Filesystem -> MashString(filesystem, PathClass),
        Total -> MashNumber(total, BytesClass),
        Used -> MashNumber(used, BytesClass),
        Available -> MashNumber(available, BytesClass),
        Mountpoint -> MashString(mountpoint, PathClass)),
      DiskSpaceEntryClass)
  }

  private def runDf(): String = {
    val builder = new ProcessBuilder("df", "-k")
    val process = builder.redirectOutput(ProcessBuilder.Redirect.PIPE).start()
    val output = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
    process.waitFor()
    output
  }

  override def summaryOpt: Option[String] = Some("Information about disk space on various partitions")

  override def typeInferenceStrategy: TypeInferenceStrategy = Seq(DiskSpaceEntryClass)

}
