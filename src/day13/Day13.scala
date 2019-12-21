package day13

import util.File

import scala.collection.mutable.ListBuffer

object Day13 {

  val program = File.fromFile("src/day13/day13.txt").head.split(',').map(s => BigInt(s.toString))

  var curX:Int = -1
  var curY:Int = -1

  val triples = ListBuffer[(Int, Int, Int)]()

  var numOutput = 0
  var numBlocks = 0

  def main(args: Array[String]): Unit = {
    val computer = new IntcodeComputer(program, onOutput).run()

    val blocks = triples.filter(_._3 == 2)

    println(blocks)
    println("Num output " + numOutput + ", " + numBlocks)
  }

  def onOutput(output:Int):Unit = {
    numOutput += 1
    //print(output + " ")
    if (numOutput % 3 == 0) {
      print(output + " ")
    }
    if (curX == -1) {
      curX = output
    } else if (curY == -1) {
      curY = output
    } else {
      triples += ((curX, curY, output))
      curX = -1
      curY = -1
    }
  }

}


