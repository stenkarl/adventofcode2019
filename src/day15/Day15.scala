package day15

import day15.Day15.direction
import util.File

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Day15 {

  val program = File.fromFile("src/day15/day15.txt").head.split(',').map(s => BigInt(s.toString))

  val empty = Point(0, 0, " ")
  val start = Point(0, 0, "S")
  val explored = mutable.Set[Point](start)
  val completed = mutable.Set[Point]()

  var curX = 0
  var curY = 0
  var direction = 0

  var lastOutput = 0

  var cur = start

  var askForInput = false

  def main(args: Array[String]): Unit = {
    val computer = new IntcodeComputer(program, onInput, onOutput)

    computer.run()
  }

  def onInput():Int = {
    direction = StdIn.readInt()
    direction
    /*
    val input = if (curX <= -12) {
      askForInput = true
      direction = StdIn.readInt()
      direction
    } else {*/
      //direction = cur.nextDirection()
      //direction
    //}
    //input
  }
    //}

  def isComplete:Boolean = {
    for (p <- explored) {
      if (p.which == ".") {
        val up = find(p.x, p.y - 1)
        if (up.isEmpty) {
          return false
        }
        val down = find(p.x, p.y + 1)
        if (down.isEmpty) {
          return false
        }
        val left = find(p.x - 1, p.y)
        if (left.isEmpty) {
          return false
        }
        val right = find(p.x + 1, p.y)
        if (right.isEmpty) {
          return false
        }
      }
    }
    true
  }

  def find(x:Int, y:Int):Option[Point] = {
    explored.find(p => p.x == x && p.y == y)
  }

  def onOutput(output:Int):Unit = {
    //println(explored.size)
    lastOutput = output
    val p:Point = if (output == 0) {
      nextPoint("#")
    } else if (output == 1) {
      val temp = nextPoint(".")
      curX = temp.x
      curY = temp.y
      temp
    } else if (output == 2) {
      println ("Found it! " + curX + ", " + curY + " " + direction)

      nextPoint("$")
    } else {
      println("ERROR")
      System.exit(0)
      null
    }
    explored += p
    cur = explored.find(pt => p.x == pt.x && p.y == pt.y).get

    //if (p.which == "$") {
      //printMap()
      //System.exit(0)
    //}
    printMap()
    if (isComplete) {
      printMap()
      System.exit(0)
    }
    /*
    if (askForInput) {
      printMap()
    }*/
  }

  def nextPoint(which:String):Point = {
    direction match {
      case 1 => Point(curX, curY - 1, which)
      case 2 => Point(curX, curY + 1, which)
      case 3 => Point(curX - 1, curY, which)
      case 4 => Point(curX + 1, curY, which)
    }
  }

  def printMap(): Unit = {
    println(explored)
    val minX:Int = explored.minBy(_.x).x
    val maxX:Int = explored.maxBy(_.x).x
    val minY:Int = explored.minBy(_.y).y
    val maxY:Int = explored.maxBy(_.y).y

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        val p:Option[Point] = explored.find(p => p.x == x && p.y == y)
        val which = p.getOrElse(empty).which
        if (x == curX && y == curY) {
          print("^")
        } else if (x == 0 && y == 0) {
          print("S")
        } else {
          print(which)
        }
      }
      println()
    }
  }

}

case class Point(x:Int, y:Int, which:String = " ") {

  var dir = 0
  val path = ListBuffer[Int]()

  def nextDirection(): Int = {
    dir = dir match {
      case 1 => 4
      case 2 => 3
      case 3 => 1
      case _ => 2
    }
    dir
  }
}
