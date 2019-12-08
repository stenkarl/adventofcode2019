package day6

import util.File

import scala.collection.mutable.MutableList

object Day6 {

  def main(args: Array[String]): Unit = {

    val nodes = File.fromFile("src/day6/day6.txt").
                    map(_.split("\\)").toList).map(n => Node(n(1), n(0)))

    for (n <- nodes) {
      val parentOption = nodes.find(_.value == n.parentName)
      if (parentOption.isDefined) {
        n.parent = parentOption.get
      } else {
        println("Missing parent for " + n)
      }
    }
    val sum = nodes.foldLeft(0)((acc, n) => {
      val p = n.path()
      println("n(" + n.value + ") = " + p)
      acc + p
    })

    println("sum " + sum)

  }

}

case class Node(value:String, parentName:String, children:MutableList[Node] = MutableList()) {

  var parent:Node = null

  def find(target:String): Node = {
    if (target == value) return this
    else {
      for (ch <- children) {
        val found = ch.find(target)
        if (found != null) return found
      }
    }
    null
  }

  def path():Int = {
    var hops = 1
    var p = parent
    while (p != null) {
      hops += 1
      p = p.parent
    }
    hops
  }
}