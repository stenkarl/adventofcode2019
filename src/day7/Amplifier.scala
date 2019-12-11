package day7

class Amplifier(val name:String, phaseSetting:Int, program:Array[Int]) {
  var pc = 0

  var inputSetting = 0
  var phasePrompt = true
  var output = 0

  var outputReady = false
  var done = false

  def run(inputSetting:Int):Int = {
    this.inputSetting = inputSetting
    outputReady = false
    while (program(pc) != 99 && !outputReady) {
      processOp()
    }
    done = program(pc) == 99
    output
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

    //println("Jump If True (" + param1 + " m = " + mode1 + ", " +
    //  param2 + " m = " + mode2 + ") = " + shouldJump);
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

    //println("Jump If False (" + param1 + " m = " + mode1 + ", " +
    //  param2 + " m = " + mode2 + ") = " + shouldJump);
  }

  def doEqual(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val isEqual = if (param1 == param2) 1 else 0
    program(program(pc + 3)) = isEqual

    //println("Equal (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") = " + isEqual + " at " + program(pc + 3));

    pc += 4
  }

  def doLess(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val isLess = if (param1 < param2) 1 else 0
    program(program(pc + 3)) = isLess

    //println("Less (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") < " + isLess + " at " + program(pc + 3));

    pc += 4
  }

  def doAdd(): Unit = {
    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)

    val sum = param1 + param2
    program(program(pc + 3)) = sum

    //println("Add (" + param1 + " m = " + mode1 + ", " + param2 + " m = " + mode2 + ") = " + sum + " at " + program(pc + 3));

    pc += 4
  }

  def doMult() = {

    val mode1 = parameterMode(1)
    val mode2 = parameterMode(2)

    val param1 = if (mode1 == 0) valueAt(1) else immediateAt(1)
    val param2 = if (mode2 == 0) valueAt(2) else immediateAt(2)
    val product = param1 * param2
    program(program(pc + 3)) = product
    //println("Mult (" + param1 + ", " + param2 + ") = " + product + " at " + program(program(pc + 3)));

    pc += 4
  }

  def doInput() = {
    //println("doInput " + phasePrompt + ", " + phaseSettings(phaseIndex) + ", " + inputSetting)
    val input = if (phasePrompt) {
      phasePrompt = false
      phaseSetting
    } else {
      inputSetting
    }
    val dest = program(pc + 1)
    program(dest) = input
    pc += 2
  }

  def doOutput() = {
    val result = valueAt(1)
    output = result
    //println(result)
    pc += 2
    outputReady = true
  }

}

