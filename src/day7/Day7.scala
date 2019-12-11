package day7

import util.File

import scala.io.StdIn

object Day7 {
  private val rawFile = File.fromFile("src/day7/day7.txt")(0)
  private val program = rawFile.split(',').map(_.toString.toInt)

  val phaseSettings:List[Int] = List(9,7,8,5,6)

  var amplifiers = List[Amplifier]()

  def main(args: Array[String]): Unit = {
    val permutations = phaseSettings.permutations
    var highest = 0
    var numPerms = 0
    for (p <- permutations) {
      initAmplifiers(p)
      println (p)
      val result = runProgram()
      if (result > highest) {
        highest = result
      }
      numPerms += 1

    }
    println("Num Perms " + numPerms + ", Highest " + highest)
  }

  def initAmplifiers(ps:List[Int]) = {
    amplifiers = List(new Amplifier("A", ps(0), program.clone()),
      new Amplifier("B", ps(1), program.clone()),
      new Amplifier("C", ps(2), program.clone()),
      new Amplifier("D", ps(3), program.clone()),
      new Amplifier("E", ps(4), program.clone()))
  }

  def runProgram(): Int = {
    var ampIndex = 0
    var nextInput = 0
    while (!allDone) {
      val curAmp = amplifiers(ampIndex)
      //println("run amp " + curAmp.name + " with input " + nextInput)
      nextInput = curAmp.run(nextInput)
      ampIndex += 1
      if (ampIndex >= amplifiers.size) {
        ampIndex = 0
      }
    }
    nextInput
  }

  def allDone = !amplifiers.exists(!_.done)

}
