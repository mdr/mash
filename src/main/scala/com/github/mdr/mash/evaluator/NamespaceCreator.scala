package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.HasName
import com.github.mdr.mash.ns.MashRoot
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object NamespaceCreator {

  def createNamespace: MashObject = {
    val allObjects = MashRoot.AllFunctions ++ MashRoot.AllClasses
    createNamespace(allObjects.flatMap(makeThings))
  }

  private def makeThings(hasName: HasName with MashValue): Seq[Thing] =
    hasName.aliases.map { alias ⇒ Thing(alias.segments, hasName) } :+ Thing(hasName.segments, hasName)

  private case class Thing(segments: Seq[String], value: MashValue) {
    def first: String = segments.head
    def rest: Thing = Thing(segments.tail, value)
    def isLeaf = segments.size == 1
  }

  private def createNamespace(things: Seq[Thing]): MashObject = {
    val thingsByName: Map[String, Seq[Thing]] = things.groupBy(_.first)
    val fields: Seq[(String, MashValue)] =
      thingsByName.map {
        case (name, things) ⇒
          val value = things match {
            case Seq(thing) if thing.isLeaf ⇒
              thing.value
            case _ ⇒
              require(!things.exists(_.isLeaf), "No leaf exists in " + things)
              createNamespace(things.map(_.rest))
          }
          name -> value
      }.toSeq.sortBy(_._1)
    MashObject.of(fields)
  }

}