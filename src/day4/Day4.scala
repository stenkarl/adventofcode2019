package day4

object Day4 {

  val lower = 272091
  val upper = 815432

  def main(args: Array[String]): Unit = {
    //hasDups(111122)

    var count = 0;
    for (i <- lower until upper) {
      if (ascending(i) && hasDups(i)) {
        count += 1
        println(i)
      }
    }
    println("count " + count + ", " + (upper - lower))


  }

  def ascending(num:Int):Boolean = {
    for (i <- 0 until 5) {
      val first = num.toString.charAt(i).toInt
      val second = num.toString.charAt(i + 1).toInt
      if (second < first) {
        return false
      }
    }
    true
  }

  def hasDups(num:Int):Boolean =  {
    var isDup = false
    var tooLong = false
    for (i <- 0 until 5) {
      val first = num.toString.charAt(i).toString.toInt
      val second = num.toString.charAt(i + 1).toString.toInt
      println(first + ", " + second)
      if (first == second && !isDup && i == 4) {
        //println("return true (end of string)")
        return true
      } else if (first == second && !isDup) {
        //println("first == second && !isDup")
        isDup = true
      } else if (first != second && isDup && !tooLong) {
        //println("returning true")
        return true
      } else if (first == second && isDup) {
        tooLong = true
      } else if (isDup) {
        isDup = false
        tooLong = false
      }
    }
    false
  }

}
