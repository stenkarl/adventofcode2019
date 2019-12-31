package day16

import util.File

object Day16 {

  val inputStr = File.fromFile("src/day16/day16.txt").head
  //val inputStr = "12345678"
  //val inputStr = "69317163492948606335995924319873"
  val input = inputStr.map(_.toString.toInt).toList

  val basePattern = List(0, 1, 0, -1)

  def main(args: Array[String]): Unit = {
    println(input)

    var output = input
    for (i <- 1 to 100) {
      output = phase(output)
      println(i + " => " + output.take(8).mkString(""))
    }
    println(output.take(8).mkString(""))
  }

  def createPattern(element:Int):List[Int] = {
    val list = for (n <- basePattern;
          i <- 0 until element
     ) yield n

    list.tail :+ list.head
  }

  def phase(in:List[Int]):List[Int] = {
    (for (i <- in.indices) yield {
      val pattern = createPattern(i + 1)
      val sum = (for (j <- in.indices) yield {
        val first = in(j)
        val other = pattern(j % pattern.size)
        val product = first * other

        //print(first + "*" + other + "=" + product + " ")
        product
      }).sum.toString.last
      //println()
      sum.toString.toInt
    }).toList
  }

}
