package day10

import util.File

object Day10 {

  def main(args: Array[String]): Unit = {
    val file = File.fromFile("src/day10/day10.txt")

    val grid = new Grid(file)

    //println(grid)

    val visible = grid.mostVisible()
    println("Most Visible " + grid.visible(visible))

    //println(grid.gcf(18, 12))
  }

}
