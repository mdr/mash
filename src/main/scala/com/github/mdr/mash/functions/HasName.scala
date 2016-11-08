package com.github.mdr.mash.functions

case class Namespace(segments: Seq[String]) {

  override def toString = segments.mkString(".")

}

object FullyQualifiedName {

  def of(s: String): Option[FullyQualifiedName] = {
    val chunks = s.split("\\.")
    for (name ← chunks.lastOption)
      yield FullyQualifiedName(Namespace(chunks.init), name)
  }

  def apply(s: String): FullyQualifiedName = of(s).getOrElse(throw new RuntimeException("Not a FQN: " + s))

}

case class FullyQualifiedName(namespace: Namespace, name: String) {

  override def toString = if (namespace.segments.isEmpty) name else namespace + "." + name

  def segments: Seq[String] = namespace.segments :+ name

}

trait HasName {

  def nameOpt: Option[String]

  def namespaceOpt: Option[Namespace]

  def namespace = namespaceOpt.getOrElse(Namespace(Seq()))

  def name = nameOpt.getOrElse("anonymous")

  def segments: Seq[String] = namespaceOpt.toSeq.flatMap(_.segments) ++ nameOpt

  def fullyQualifiedName = FullyQualifiedName(namespace, name)

  def aliases: Seq[FullyQualifiedName] = Seq()

}
