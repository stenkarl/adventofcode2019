package day2

import util.File
import scala.util.control.Breaks._

object Day2 {

  var pc = 0
  val rawFile = File.fromFile("src/day2/day2.txt")(0)
  var program = rawFile.split(',').map(_.toString.toInt)
  //val program:Array[Int] = Array(1,1,1,4,99,5,6,0,99)

  val targetOutput = 19690720

  def main(args: Array[String]): Unit = {
    for (n <- 0 to 99; v <- 0 to 99) {
      pc = 0
      program = rawFile.split(',').map(_.toString.toInt)
      val ret = run(n, v)
      if (ret == targetOutput) {
        println("n = " + n + ", v = " + v)
        break
      }
    }
  }

  def run(noun:Int, verb:Int):Int = {
    program(1) = noun
    program(2) = verb
    //println(program.mkString(","))
    while (program(pc) != 99) {
      processOp()
    }
    println(program.mkString(","))

    program(0)
  }

  def processOp(): Unit = {
    val op = program(pc)
    if (op == 99) {
      return
    }
    if (op == 1) {
      doAdd()
    } else if (op == 2) {
      doMult()
    }
    pc += 4
  }

  def valueAt(offset:Int): Int = program(program(pc + offset))

  def doAdd(): Unit = {
    val sum = valueAt(1) + valueAt(2)
    program(program(pc + 3)) = sum
  }

  def doMult() = {
    val product = valueAt(1) * valueAt(2)
    program(program(pc + 3)) = product
  }

}
