package day1

import util.File

object Day1 {

  def main(args: Array[String]): Unit = {
    println("Day1")
    val massList = File.fromFile("src/day1/day1.txt").map(_.toLong)

    //val sum = massList.foldLeft(0L){(acc, mass) => acc + calculateTotal(mass)}

    val part1 = massList.fold(0L) { (acc, mass) => acc + (mass / 3 - 2)}

    val map = massList.map (recursiveCalculate)
    val reduce = map.reduce { (first, second) => first + second}
    val sum = map.sum

    println(sum)
    println(recursiveCalculate(100756L))
  }

  def calculate(mass:Long):Long = {
    val m = Math.max(mass / 3 - 2, 0)
    //println("mass " + mass + ", fuel " + m)
    m
  }

  def recursiveCalculate(mass:Long):Long = {
    val curFuel = calculate(mass)
    //println("mass " + mass + ", curFuel " + curFuel)
    if (curFuel <= 0) return 0
    else recursiveCalculate(curFuel) + curFuel
  }

  def calculateTotal(mass:Long):Long = {
    var curFuel = calculate(mass)
    var totalFuel = curFuel
    do {
      curFuel = calculate(curFuel)
      if (curFuel > 0) {
        totalFuel += curFuel
      }
    } while (curFuel > 0)
    totalFuel
  }

}
