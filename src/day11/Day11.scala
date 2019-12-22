package day11

import util.File

import scala.collection.mutable.ListBuffer

object Day11 {

  val program = File.fromFile("src/day11/day11.txt").head.split(',').map(s => BigInt(s.toString))

  val directions = List(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0))

  var cur = Point(0, 0)
  val tiles = ListBuffer[Point](cur)
  var dir = 0

  var paintMode = true

  def main(args: Array[String]): Unit = {

    val output = new IntcodeComputer(program, inputHandler, outputHandler).run()

    print(output)

    printTiles()
  }

  def inputHandler():Int = {
    val input = if (tiles.contains(cur)) 1 else 0

    println("input " + input)

    input
  }

  def outputHandler(output:Int):Unit = {
    println("output = " + output)

    if (paintMode) {
      paint(output)
    } else {
      turn(output)
      move()
    }
    paintMode = !paintMode


  }

  def paint(color:Int): Unit = {
    if (color == 1) {
      println("Coloring " + cur + " white")
      if (!tiles.contains(cur)) {
        tiles += cur
        println("Adding " + cur + " to white list. Size is " + tiles.size)
      }
    } else {
      println("Coloring " + cur + " black")
      if (tiles.contains(cur)) {
        tiles -= cur
        println("Removing " + cur + " from white list")
      }
    }
    //println("White colored tiles " + tiles)
  }

  def move() = {
    val direction = directions(dir)
    cur = Point(cur.x + direction.x, cur.y + direction.y)
    println("Moving to " + cur + " in direction " + direction)
  }

  def turn(d:Int) = {
    val amt = if (d == 0) -1 else 1
    dir += amt

    if (dir < 0) {
      dir = directions.size - 1
    } else if (dir >= directions.size) {
      dir = 0
    }
    println(d + " turns to " + directions(dir))
  }

  def printTiles(): Unit = {
    val minX:Int = tiles.minBy(_.x).x
    val maxX:Int = tiles.maxBy(_.x).x
    val minY:Int = tiles.minBy(_.y).y
    val maxY:Int = tiles.maxBy(_.y).y

    println("min/max " + minX +", " + maxX + ", " + minY + ", " + maxY + ", size " + tiles.size)

    for (y <- maxY to minY by -1) {
      for (x <- minX to maxX) {
        if (tiles.contains(Point(x, y))) {
          print("#")
        } else {
          print(" ")
        }

      }
      println()
    }
  }

}

case class Point(x:Int, y:Int)
