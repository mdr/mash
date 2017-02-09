package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.ns.core.{ BytesClass, NumberClass, StringClass }

object DiskSpaceEntryClass extends MashClass("os.DiskSpaceEntry") {

  object Fields {
    lazy val Filesystem = Field("filesystem", Some("Name of the filesystem"), StringClass taggedWith PathClass)
    lazy val Total = Field("total", Some("Total space"), NumberClass taggedWith BytesClass)
    lazy val Used = Field("used", Some("Used"), NumberClass taggedWith BytesClass)
    lazy val Available = Field("available", Some("Available"), NumberClass taggedWith BytesClass)
    lazy val Mountpoint = Field("mountPoint", Some("Mount point"), StringClass taggedWith PathClass)
  }

  import Fields._

  override val fields = Seq(Filesystem, Total, Used, Available, Mountpoint)

  override def summaryOpt: Option[String] = Some("Information about disk space use for a partition")
}

