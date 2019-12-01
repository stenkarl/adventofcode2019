package day1

import util.File

object Day1 {

  def main(args: Array[String]): Unit = {
    println("Day1")
    val massList = File.fromFile("src/day1/day1.txt").map(_.toLong)

    val sum = massList.foldLeft(0L){(acc, mass) => acc + calculateTotal(mass)}

    println(sum)
  }

  def calculate(mass:Long):Long = mass / 3 - 2

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
