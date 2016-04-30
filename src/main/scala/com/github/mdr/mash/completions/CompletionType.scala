package com.github.mdr.mash.completions

case class CompletionType(name: String)

object CompletionType {

  val Directory = CompletionType("Directory")
  val File = CompletionType("File")
  val Flag = CompletionType("Flag")
  val Field = CompletionType("Field")
  val Method = CompletionType("Method")
  val Binding = CompletionType("Binding")
  val Function = CompletionType("Function")

}
