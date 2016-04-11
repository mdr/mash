package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashFunction
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.StandardFunctions

object NamespaceCreator {

  def createNamespace: MashObject = {
    val allFunctions = StandardFunctions.Functions ++ StandardFunctions.GitFunctions
    val allThings = allFunctions.map(makeThing) ++ StandardFunctions.AllClasses.map(makeThing)
    createNamespace(allThings)
  }

  private def makeThing(function: MashFunction): Thing =
    Thing(function.namespaceOpt.toSeq.flatMap(_.path) ++ function.nameOpt.toSeq, function)

  private def makeThing(klass: MashClass): Thing =
    Thing(klass.namespaceOpt.toSeq.flatMap(_.path) ++ klass.nameOpt.toSeq, klass)

  private case class Thing(path: Seq[String], value: Any) {
    def first: String = path.head
    def rest: Thing = Thing(path.tail, value)
    def isLeaf = path.size == 1
  }

  private def createNamespace(things: Seq[Thing]): MashObject = {
    val xs = things.groupBy(_.first)
    val ys = xs.map {
      case (name, things) ⇒
        val rhs = things match {
          case Seq(thing) if thing.isLeaf ⇒
            thing.value
          case _ ⇒
            require(!things.exists(_.isLeaf))
            createNamespace(things.map(_.rest))
        }
        name -> rhs
    }
    MashObject(ListMap(ys.toSeq.sortBy(_._1): _*), classOpt = None)
  }

}