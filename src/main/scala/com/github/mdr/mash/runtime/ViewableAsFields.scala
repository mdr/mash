package com.github.mdr.mash.runtime

import com.github.mdr.mash.classes.Field

import scala.collection.mutable.LinkedHashMap
import scala.language.implicitConversions

trait ViewableAsFields {
  def fields: LinkedHashMap[MashValue, MashValue]
}

object ViewableAsFields {

  implicit def fromLinkedHashMapStrings(map: LinkedHashMap[String, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = for ((k, v) <- map) yield MashString(k) -> v
    }

  implicit def fromLinkedHashMap(map: LinkedHashMap[MashValue, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = map
    }

  implicit def fromMapStrings(map: Map[String, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap((for ((k, v) <- map.toSeq) yield MashString(k) -> v): _*)
    }

  implicit def fromMap(map: Map[MashValue, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(map.toSeq: _*)
    }

  implicit def fromListMapFields(map: Map[Field, MashValue]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(map.toSeq.map { case (field, v) ⇒ MashString(field.name) -> v }: _*)
    }

  implicit def fromPairsStrings(pairs: Seq[(String, MashValue)]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap((for ((k, v) <- pairs) yield MashString(k) -> v): _*)
    }

  implicit def fromPairs(pairs: Seq[(MashValue, MashValue)]): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(pairs: _*)
    }

  implicit def fromPairString(pair: (String, MashValue)): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(MashString(pair._1) -> pair._2)
    }

  implicit def fromPair(pair: (MashValue, MashValue)): ViewableAsFields =
    new ViewableAsFields {
      def fields = LinkedHashMap(pair._1 -> pair._2)
    }

}
