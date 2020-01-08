package day19

import util.File

import scala.io.StdIn

object Day19 {

  val program = File.fromFile("src/day19/day19.txt").head.split(',').map(s => BigInt(s.toString))

  var curX = 0
  var curY = 0

  var needsX = true

  val width = 50

  var numOnes = 0

  def main(args: Array[String]): Unit = {

    part1()
  }

  def part1(): Unit = {
    for (y <- 0 until width) {
      for ( x <- 0 until width) {
        def computer = new IntcodeComputer(program, onInput, onOutput)

        curY = y
        curX = x
        computer.run()
      }
    }
    println("Num Ones: " + numOnes)
  }

  def onInput():Int = {
    val xOrY = if (needsX) curX else curY

    needsX = !needsX

    xOrY
  }

  def onOutput(out:Int):Unit = {
    //println("(" + curX + "," + curY + ") " + out)
    print(out)
    if (out == 1) {
      numOnes += 1
    }
    if (curX == width - 1) {
      println()
    }
  }

}
