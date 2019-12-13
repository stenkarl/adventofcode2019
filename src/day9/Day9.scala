package day9

import util.File

object Day9 {

  //val program = Array[BigInt](109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
  //val program:Array[BigInt] = Array[BigInt](104, BigInt("1125899906842624"),99)
  //val program = Array[BigInt](1102,34915192,34915192,7,4,7,99,0)
  val program = File.fromFile("src/day9/day9.txt").head.split(',').map(s => BigInt(s.toString))


  def main(args: Array[String]): Unit = {
    println(program.mkString(","))
    val output = new IntcodeComputer(program).run()

    //println(output)
  }

}
