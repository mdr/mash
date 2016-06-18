package com.github.mdr.mash.runtime

sealed trait MashUnit extends MashValue
case object MashUnit extends MashUnit