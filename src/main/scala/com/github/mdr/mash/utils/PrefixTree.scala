package com.github.mdr.mash.utils

case class PrefixTree[T] private (root: Node[T]) {

  def add(s: String, item: T): PrefixTree[T] =
    PrefixTree(root.add(s.toList, item))

  def get(s: String): Option[T] = root.get(s.toList)

  def get(charSource: ⇒ Char): Either[Seq[Char], T] =
    root.get(charSource, Seq())
}

object PrefixTree {

  def empty[T]: PrefixTree[T] = PrefixTree(Node.Internal[T](Map()))

  def apply[T](xs: (String, T)*): PrefixTree[T] =
    xs.foldLeft(empty[T]) { case (tree, (s, item)) ⇒ tree.add(s, item) }

}

sealed trait Node[T] {
  def add(x: List[Char], item: T): Node[T]
  def get(chars: List[Char]): Option[T]
  def get(charSource: ⇒ Char, acc: Seq[Char]): Either[Seq[Char], T]
}

object Node {

  case class Internal[T](cs: Map[Char, Node[T]] = Map()) extends Node[T] {

    def get(chars: List[Char]): Option[T] = chars match {
      case Nil      ⇒ None
      case (h :: t) ⇒ cs.get(h).flatMap(_.get(t))
    }

    def get(charSource: ⇒ Char, acc: Seq[Char]): Either[Seq[Char], T] = {
      val c = charSource
      cs.get(c) match {
        case Some(childNode) ⇒ childNode.get(charSource, acc :+ c)
        case None            ⇒ Left(acc)
      }
    }

    def add(chars: List[Char], item: T): Node[T] = chars match {
      case Nil ⇒
        Leaf(item)
      case (h :: t) ⇒
        val childNode = cs.getOrElse(h, Internal(Map()))
        val newChildNode = childNode.add(t, item)
        Internal(cs + (h -> newChildNode))
    }

  }

  case class Leaf[T](item: T) extends Node[T] {

    def add(chars: List[Char], item: T): Node[T] =
      throw new IllegalArgumentException("Non unique char sequence for " + item)

    def get(chars: List[Char]): Option[T] = chars match {
      case Nil      ⇒ Some(item)
      case (h :: t) ⇒ None
    }

    def get(charSource: ⇒ Char, acc: Seq[Char]): Either[Seq[Char], T] = Right(item)
  }

}