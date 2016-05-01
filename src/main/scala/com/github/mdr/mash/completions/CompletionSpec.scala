package com.github.mdr.mash.completions

import com.github.mdr.mash.inference.Type

sealed trait CompletionSpec

object CompletionSpec {

  case object Directory extends CompletionSpec
  case object File extends CompletionSpec
  case class Members(targetType: Type) extends CompletionSpec

}
