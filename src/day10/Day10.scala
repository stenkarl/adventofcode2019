package day10

import util.File

object Day10 {

  val center = Point(11, 13)

  def main(args: Array[String]): Unit = {
    val file = File.fromFile("src/day10/day10.txt")

    val grid = new Grid(file)

    //println(grid)

    //val visible = grid.mostVisible()
    //println(visible)
    val asteroids = grid.visible(center).sortBy(angle(_)).reverse

    println("Top " + angle(Point(11, 20)))
    println("Right " + angle(Point(20, 13)))
    println("Bottom " + angle(Point(11, -200)))
    println("Bottom Left " + angle(Point(0, 14)))
    println("Left " + angle(Point(-50, 13)))

    var i = 0
    asteroids.foreach { it =>
      i += 1
      println(i + " " + it + ", " + angle(it) + " " + (it.x * 100 + it.y))
    }

    println("max " + asteroids.maxBy(angle(_)))

    println("angle " + angle(Point(10, 13)))
/*
    var last = Point(0,0)
    asteroids.foreach { it =>
      val a = angle(it)
      if (a < 270.0 && a > angle(last)) {
        last = it
      }
    }*/
    //println("last " + last + ", " + angle(last))

    //println(grid.gcf(18, 12))

    println(print(asteroids))
  }

  def angle(p:Point):Double = {
    val delta_x = p.x - center.x
    val delta_y = p.y - center.y
    val theta_radians = Math.atan2(delta_y, delta_x)

    val rads1 = theta_radians + (Math.PI / 2.0)
    val rads = if (rads1 < 0) Math.abs(rads1) else 2 * Math.PI - rads1

    Math.toDegrees(rads)
  }

  def print(asteroids:List[Point]) = {
    val str = new StringBuilder("")
    val width, height = 21
    val top = asteroids.take(10)
    for (y <- 0 until width) {
      for (x <- 0 until height) {
        val char = if (Point(x,y) == center) "X"
        else if (top.contains(Point(x, y))) "" + top.indexOf(Point(x,y))
        else if (asteroids.contains(Point(x, y))) "#"
        else " "
        str.append(char)
      }
      str.append("\n")
    }
    str.toString()
  }

}
