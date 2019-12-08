package day5

import util.File

import scala.io.StdIn

object Day5 {
  var pc = 0
  val rawFile = File.fromFile("src/day5/day5.txt")(0)
  val program = rawFile.split(',').map(_.toString.toInt)
  /*
  val program:Array[Int] = Array(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
*/
  def main(args: Array[String]): Unit = {
    pc = 0
    run()
  }

  def run():Int = {
    //println(program.mkString(","))
    while (program(pc) != 99) {
      processOp()
    }
    //println(program.mkString(","))

    program(0)
  }

  def processOp(): Unit = {
    val op = parseOp(program(pc))
    //println("op is " + op)
    if (op == 99) {
      return
    }
    if (op == 1) {
      doAdd()
    } else if (op == 2) {
      doMult()
    } else if (op == 3) { // input
      doInput()
    } else if (op == 4) { // output
      doOutput()
    } else if (op == 5) {
      doJumpIfTrue()
    } else if (op == 6) {
      doJumpIfFalse()
    } else if (op == 7) {
      doLess()
    } else if (op == 8) {
      doEqual()
    } else {
      println("Unknown Op: " + op)
      System.exit(1)
    }

  }

  def parseOp(code:Int):Int = {
    val str = code.toString
    if (str.length == 1) return code
    str.substring(str.length - 2).toInt
  }

  def parameterMode(num:Int):Int = {
    val str = program(pc).toString

    if (str.length == 1) return 0
    if (str.length == 3 && num == 2) return 0
    //println("pm " + str)
    str.charAt(str.length - (2 + num)).toString.toInt
  }

  def valueAt(offset:Int): Int = program(program(pc + offset))

  def immediateAt(offset:Int): Int = program(pc + offset)

  def doJumpIfTrue(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val shouldJump = param1 != 0
    if (shouldJump) {
      pc = param2
    } else {
      pc += 3
    }

    println("Jump If True (" + param1 + " m = " + mode1 + ", " +
              param2 + " m = " + mode2 + ") = " + shouldJump);
  }

  def doJumpIfFalse(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val shouldJump = param1 == 0
    if (shouldJump) {
      pc = param2
    } else {
      pc += 3
    }

    println("Jump If False (" + param1 + " m = " + mode1 + ", " +
      param2 + " m = " + mode2 + ") = " + shouldJump);
  }

  def doEqual(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val isEqual = if (param1 == param2) 1 else 0
    program(program(pc + 3)) = isEqual

    println("Equal (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") = " + isEqual + " at " + program(pc + 3));

    pc += 4
  }

  def doLess(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val isLess = if (param1 < param2) 1 else 0
    program(program(pc + 3)) = isLess

    println("Less (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") < " + isLess + " at " + program(pc + 3));

    pc += 4
  }

  def doAdd(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val sum = param1 + param2
    program(program(pc + 3)) = sum

    println("Add (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") = " + sum + " at " + program(pc + 3));

    pc += 4
  }

  def doMult() = {

    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)
    val product = param1 * param2
    program(program(pc + 3)) = product
    println("Mult (" + param1 + ", " + param2 + ") = " + product + " at " + program(program(pc + 3)));

    pc += 4
  }

  def doInput() = {
    print("input> ")
    val input = StdIn.readInt()
    val dest = program(pc + 1)
    program(dest) = input
    pc += 2
  }

  def doOutput() = {
    println(valueAt(1))
    pc += 2
  }

}
