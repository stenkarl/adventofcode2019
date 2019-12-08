package day6

import util.File

import scala.collection.mutable.MutableList

object Day6 {

  def main(args: Array[String]): Unit = {

    val nodes = Node("COM", null) :: File.fromFile("src/day6/day6.txt").
      map(_.split("\\)").toList).map(n => Node(n(1), n(0)))

    for (n <- nodes) {
      val parentOption = nodes.find(_.value == n.parentName)
      if (parentOption.isDefined) {
        n.parent = parentOption.get
        n.parent.children += n
      } else {
        println("Missing parent for " + n)
      }
    }
    val com = nodes.find(_.value == "COM").get
    println("COM " + com.printPath())

    val ccb = nodes.find(_.value == "CCB").get
    println("CCB " + ccb.printPath())

    val ww8 = nodes.find(_.value == "WW8").get
    println("WW8 " + ww8.printPath())

    println("path " + com.pathTo(ww8))
    partTwo(nodes)

  }

  def partTwo(nodes:List[Node]): Unit = {
    val you = nodes.find(_.value == "YOU").get
    val san = nodes.find(_.value == "SAN").get
    val ccb = nodes.find(_.value == "CCB").get

    println("path back " + you.printPathBackTo(ccb))
    println("path back " + san.printPathBackTo(ccb))

  }

  def partTwoA(nodes:List[Node]) {
    val you = nodes.find(_.value == "YOU").get
    val san = nodes.find(_.value == "SAN").get
    println(you.printPath())
    println(san.printPath())

    var found = false
    var p = you.parent
    while (!found) {
      if (p.find( "SAN") != null) {
        found = true
        println("Found SAN at node: " + p.value)
      } else {
        if (p.parent == null) {
          println("ERROR, Null parent in " + p)
        }
        p = p.parent
      }
    }
  }

  def partOne(nodes:List[Node]) = {
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

  def pathTo(node:Node):Int = {
    if (node.value == value) return 1
    for (ch <- children) {
      return 1 + ch.pathTo(node)
    }
    return 1
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

  def printPath(): String = {
    doPrintPath(1)
  }

  def printPathBackTo(node:Node): String = {
    doPrintPathBackTo(node, 1)
  }

  private def doPrintPathBackTo(node:Node, level:Int): String = {
    if (parent == null || parent == node) {
      return parent.value
    }
    parent.doPrintPathBackTo(node, level + 1) + " -> (" + level + ") " + value
  }

  private def doPrintPath(level:Int): String = {
    if (parent != null) {
      return parent.doPrintPath(level + 1) + " -> (" + level + ") " + value
    }
    value
  }

  @Override override def toString: String = "Node(" + value + ", children:" +
                                          children.size + ")"
}