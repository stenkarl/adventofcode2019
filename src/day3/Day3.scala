package day3

import util.File

object Day3 {

  val input = File.fromFile("src/day3/day3.txt")

  def main(args: Array[String]): Unit = {
    //println(input)

    val i1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    val i2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

    val first = inputToInstructions(input(0))
    val second = inputToInstructions(input(1))

    println(first)
    println(second)

    val firstPath = createPath(first) //.distinct
    val secondPath = createPath(second) //.distinct

    println(firstPath)
    println(secondPath)

    val intersection = firstPath.intersect(secondPath).tail
    println(intersection)
    val min = intersection.minBy(p => Math.abs(p.y) + Math.abs(p.x))

    println (min + ", distance " + (Math.abs(min.y) + Math.abs(min.x)))

    val distances = for (i <- intersection) yield {
      val p1 = firstPath.indexOf(i)
      val p2 = secondPath.indexOf(i)
      val sum = p1 + p2

      println(i + ", " + p1 + " + " + p2 + " = " + sum)

      sum
    }

    println(distances.min)
  }

  def createPath(instructions:List[Instruction]):List[Point] = {
    var last = Point(0, 0)

    val path = for (inst <- instructions) yield {
      val subPath = inst.direction match {
        case "U" => for (i <- 1 to inst.distance) yield Point(last.x, last.y + i)
        case "D" => for (i <- 1 to inst.distance) yield Point(last.x, last.y - i)
        case "L" => for (i <- 1 to inst.distance) yield Point(last.x - i, last.y)
        case "R" => for (i <- 1 to inst.distance) yield Point(last.x + i, last.y)
      }
      last = subPath.last
      subPath
    }
    val flat = path.flatten

    Point(0, 0) :: flat
  }

  def inputToInstructions(wire:String) = wire.split(",").
                                    map(it => Instruction(it.charAt(0).toString,
                                        it.substring(1).toInt)).toList

}

case class Instruction(direction:String, distance:Int)
case class Point(x:Int, y:Int)
