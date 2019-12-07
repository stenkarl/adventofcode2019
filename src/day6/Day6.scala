package day6

import util.File

import scala.collection.mutable.MutableList

object Day6 {

  def main(args: Array[String]): Unit = {

    val root = Node("COM")

    val nodes = File.fromFile("src/day6/day6.txt").map(_.split("\\)").toList)
    for (n <- nodes) {
      val parent = root.find(n(0))
      if (parent != null) {
        parent.children += Node(n(1))
      }
    }

    println(nodes)
    //val root = Node("COM", MutableList(Node("B"), Node("C")))

    println(root)
    //println(root.path())
  }

}

case class Node(value:String, children:MutableList[Node] = MutableList()) {

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

  def path(sum:Int):Int = {
    var acc = 0
    children.foreach { it =>
      acc += 1 + it.path(sum)
    }
    return acc
  }
}