package day17

import util.File

import scala.collection.mutable.ListBuffer

object Day17 {

  val program = File.fromFile("src/day17/day17.txt").head.split(',').map(s => BigInt(s.toString))

  val track = ListBuffer[Point]()

  var x = 0
  var y = 0
  var start = Point(0, 0)

  def main(args: Array[String]): Unit = {
    print(" ")
    new IntcodeComputer(program, input, output).run()
    println(track.size)
    findIntersections()
  }

  def input():Int = {
    0
  }

  def output(out:Int):Unit = {
    val char = out.toChar
    print(char + " ")
    if (char == '\n') {
      y += 1
      x = 0
    } else {
      if (char == '#') {
        track += Point(x, y)
      } else if (char == '^') {
        start = Point(x, y)
      }
      x += 1
    }
  }

  def findIntersections():Unit = {
    val intersections = track.filter(isIntersection)
    println(intersections)

    val sum = intersections.foldLeft(0)((acc, p) => acc + (p.x * p.y))

    println(sum)
  }

  def isIntersection(p:Point):Boolean =
    track.contains(Point(p.x + 1, p.y)) &&
        track.contains(Point(p.x - 1, p.y)) &&
        track.contains(Point(p.x, p.y - 1)) &&
        track.contains(Point(p.x, p.y + 1))

}

case class Point(x:Int, y:Int)
