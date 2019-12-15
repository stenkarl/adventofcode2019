package day10

import scala.collection.mutable

class Grid(raw:List[String]) {

  val asteroids = fromFile()
  val width = raw(0).size
  val height = raw.size

  def fromFile():List[Point] = {
    var list = mutable.MutableList[Point]()
    for (y <- raw.indices) {
      for (x <- raw(y).indices) {
        if (raw(y)(x) != '.') {
          list += Point(x, y)
        }
      }
    }
    list.toList
  }

  override def toString: String = {
    val str = new StringBuilder("")
    for (y <- 0 until width) {
      for (x <- 0 until height) {
        str.append(if (asteroids.contains(Point(x, y))) "#" else " ")
      }
      str.append("\n")
    }
    str.toString()
  }

  def visible(asteroid:Point):List[Point] = {
    var count = 0
    val visibleAsteroids = new mutable.MutableList[Point]()
    for (a <- asteroids) {
      if (a != asteroid) {
        if (isVisible(a, asteroid)) {
          count += 1
          visibleAsteroids += a
        }
      }
    }
    visibleAsteroids.toList
  }

  private def isVisible(first: Point, second: Point):Boolean = {
    val rise = second.y - first.y
    val run = second.x - first.x

    val slope = if (run == 0 && rise > 0) (1, 0)
                else if (run == 0 && rise < 0) (-1, 0)
                else if (rise == 0 && run > 0) (0, 1)
                else if (rise == 0 && run < 0) (0, -1)
                else reduceSlope(rise, run)

    //println(rise + "/" + run + " for " + first + " and " + second + ", reduced: " + slope)

    var target = first
    while (target != second) {
      target = Point(target.x + slope._2, target.y + slope._1)
      if (asteroids.contains(target) && target != second) {
        return false
      }
    }

    true
  }

  def reduceSlope(rise:Int, run:Int):(Int, Int) = {
    val g = gcf(rise, run)

    (rise/g, run/g)
  }

  def gcf(first:Int, second:Int): Int = {
    val max = if (first > second) Math.abs(first) else Math.abs(second)
    val min = if (first > second) Math.abs(second) else Math.abs(first)

    var cur = min
    //println ("max " + max + " min " + min + " cur " + cur + "(max % cur)" + (max % cur) +
      //"(min % cur) " + (min % cur))
    while (max % cur != 0 || min % cur != 0) {
      //println ("max " + max + " min " + min + " cur " + cur + " (max % cur)" + (max % cur) +
      //          " (min % cur) " + (min % cur))
      cur -= 1
    }
    cur
  }

}

case class Point(x:Int, y:Int)
