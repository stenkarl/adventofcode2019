package day13

import scala.collection.mutable.Map
import scala.io.StdIn

class IntcodeComputer(program:Array[BigInt], outputHandler:Int => Unit) {

  var pc:BigInt = 0

  var relativeBase:BigInt = 0
  var output:BigInt = 0

  val memory:Map[BigInt, BigInt] = scala.collection.mutable.Map[BigInt, BigInt]()

  def run():BigInt = {
    initMemory()
    while (mem(pc) != 99) {
      processOp()
    }
    output
  }

  def initMemory() = {
    for (i <- program.indices) {
      memory(i) = program(i)
    }
  }

  def processOp(): Unit = {
    val op:BigInt = parseOp(mem(pc))
    //println("op is " + op)
    if (op == 99) {
      return
    }
    if (op == 1) {
      doAdd()
    } else if (op == 2) {
      doMult()
    } else if (op == 3) {
      doInput()
    } else if (op == 4) {
      doOutput()
    } else if (op == 5) {
      doJumpIfTrue()
    } else if (op == 6) {
      doJumpIfFalse()
    } else if (op == 7) {
      doLess()
    } else if (op == 8) {
      doEqual()
    } else if (op == 9) {
      doRelativeBase()
    } else {
      println("Unknown Op: " + op)
      System.exit(1)
    }

  }

  def parameterValue(num:Int):BigInt = {
    val mode = parameterMode(num)
    val value = if (mode == 0) valueAt(num) else if (mode == 1) immediateAt(num) else relativeAt(num)

    //println("parameterValue(" + num + ") mode(" + mode + ") = " + value)

    value
  }

  def doRelativeBase() = {
    val param1 = parameterValue(1)

    relativeBase += param1

    //println("Setting relative base to " + relativeBase)

    pc += 2
  }

  def parseOp(code:BigInt):BigInt = {
    val str = code.toString
    if (str.length == 1) return code
    str.substring(str.length - 2).toInt
  }

  def parameterMode(num:Int):Int = {
    val str = mem(pc).toString

    if (str.length == 1) return 0
    if (str.length == 3 && num >= 2) return 0
    if (str.length == 4 && num == 3) return 1
    //println("pm " + str + ", num " + num)
    val pm = str.charAt(str.length - (2 + num)).toString.toInt

    if (str.length == 5) {
      //println("parameterMode: " + str + ", " + num + " = " + pm)
    }
    //println("parameterMode: " + str + ", " + num + " = " + pm)

    pm
  }

  def write(location:BigInt, value:BigInt):Unit = {
    //println("write(" + location +", " + value + ")")
    memory(location) = value
  }

  def mem(location:BigInt): BigInt = memory.getOrElse(location, 0)
  def valueAt(offset:BigInt): BigInt = mem(mem(pc + offset))
  def immediateAt(offset:BigInt): BigInt = mem(pc + offset)
  def relativeAt(offset:BigInt): BigInt = {
    val index = relativeBase + immediateAt(offset)
    //println("index " + index + " mem(index) " + mem(index))
    mem(index)
  }

  def doJumpIfTrue(): Unit = {
    val param1 = parameterValue(1)
    val param2 = parameterValue(2)

    val shouldJump = param1 != 0
    if (shouldJump) {
      pc = param2
    } else {
      pc += 3
    }

    //println("Jump If True (" + param1 + " m = " + mode1 + ", " +
    //  param2 + " m = " + mode2 + ") = " + shouldJump);
  }

  def doJumpIfFalse(): Unit = {
    val param1 = parameterValue(1)
    val param2 = parameterValue(2)

    val shouldJump = param1 == 0
    if (shouldJump) {
      pc = param2
    } else {
      pc += 3
    }

    //println("Jump If False (" + param1 + " m = " + mode1 + ", " +
    //  param2 + " m = " + mode2 + ") = " + shouldJump);
  }

  def doEqual(): Unit = {
    val param1 = parameterValue(1)
    val param2 = parameterValue(2)

    val isEqual = if (param1 == param2) 1 else 0

    val mode = parameterMode(3)
    val dest = if (mode == 1) immediateAt(3) else relativeBase + immediateAt(3)

    write(dest, isEqual)

    //println("Equal (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") = " + isEqual + " at " + program(pc + 3));

    pc += 4
  }

  def doLess(): Unit = {
    val param1 = parameterValue(1)
    val param2 = parameterValue(2)

    val isLess = if (param1 < param2) 1 else 0

    val mode = parameterMode(3)
    val dest = if (mode == 1) immediateAt(3) else relativeBase + immediateAt(3)

    write(dest, isLess)

    //println("Less (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") < " + isLess + " at " + program(pc + 3));

    pc += 4
  }

  def doAdd(): Unit = {
    val param1 = parameterValue(1)
    val param2 = parameterValue(2)

    val sum = param1 + param2
    //write(immediateAt(3), sum)
    val mode = parameterMode(3)
    val dest = if (mode == 1) immediateAt(3) else relativeBase + immediateAt(3)

    write(dest, sum)

    //println("Add (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") = " + sum + " at " + program(pc + 3));

    pc += 4
  }

  def doMult() = {
    val param1 = parameterValue(1)
    val param2 = parameterValue(2)
    val product = param1 * param2

    val mode = parameterMode(3)
    val dest = if (mode == 1) immediateAt(3) else relativeBase + immediateAt(3)

    write(dest, product)
    //println("Mult (" + param1 + ", " + param2 + ") = " + product + " at " + program(program(pc + 3)));

    pc += 4
  }

  def doInput() = {
    print("input> ")
    val input = StdIn.readInt()
    //val dest = immediateAt(1)
    val mode = parameterMode(1)
    val dest = if (mode == 1) immediateAt(1) else relativeBase + immediateAt(1)
    //println("dest = " + dest)
    write(dest, input)
    pc += 2
  }

  def doOutput() = {
    val param1 = parameterValue(1)
    output = param1
    outputHandler(output.toInt)
    //print(output + " ")
    pc += 2
  }

}
