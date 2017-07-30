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

  val RegexOsX = """^([^\s]+\s?[^\s]+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)%\s+[^/]*(.*?)\s*$""".r

  override def params = ParameterModel.Empty

  override def call(boundParams: BoundParams): MashList = {
    val process = new ProcessBuilder("df", "-k").redirectOutput(ProcessBuilder.Redirect.PIPE).start()
    val output = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
    process.waitFor()
    val lines = StringUtils.splitIntoLines(output).init
    MashList(lines.collect {
      case RegexOsX(filesystem, total, used, available, percent, mountpoint) ⇒
        import DiskSpaceEntryClass.Fields._
        MashObject.of(
          ListMap(
            Filesystem -> MashString(filesystem, PathClass),
            Total -> MashNumber(total.toLong * 1024, BytesClass),
            Used -> MashNumber(used.toLong * 1024, BytesClass),
            Available -> MashNumber(available.toLong * 1024, BytesClass),
            Mountpoint -> MashString(mountpoint, PathClass)),
          DiskSpaceEntryClass)

    })
  }


  override def summaryOpt: Option[String] = Some("Information about disk space on various partitions")

  override def typeInferenceStrategy: TypeInferenceStrategy = Seq(DiskSpaceEntryClass)


}
