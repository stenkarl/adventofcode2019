package day8

import util.File

object Day8 {

  val input = File.fromFile("src/day8/day8.txt").head

  val width = 25
  val height = 6
  val numLayers = input.length / (width * height)
  var layers = IndexedSeq[Layer]()

  def main(args: Array[String]): Unit = {
    val image = input.map(_.toString().toInt)

    println(numLayers)

    var start = 0
    layers = for (layer <- 0 until numLayers) yield {
      val data = Array.ofDim[Int](height, width)
      for (row <- 0 until height; col <- 0 until width) yield {
        val imageIndex = start + ((row * width) + col)
        println("(" + row + "," + col + ") = " + imageIndex)
        data(row)(col) = image(imageIndex)

      }
      start += width * height
      Layer(data)
    }

    val layer = layers.minBy(_.count(0))
    println(layer.count(0) + ", " + layer.count(1) + ", " + layer.count(2))
    println(layer.numOnesAndTwos())

    render()
  }

  def render(): Unit = {
    val data = Array.ofDim[Int](height, width)

    for (row <- 0 until height; col <- 0 until width) {
      val value = layers.find(cur => cur.data(row)(col) == 0 || cur.data(row)(col) == 1)
      data(row)(col) = value.get.data(row)(col)
    }

    val mergedLayer = Layer(data)

    println(mergedLayer)
  }

  case class Layer(data:Array[Array[Int]]) {

    val flatData = for (row <- 0 until height; col <- 0 until width) yield data(row)(col)

    override def toString: String = {
      val str = StringBuilder.newBuilder
      for (row <- 0 until height; col <- 0 until width) {
        str.append(if (data(row)(col) == 1) data(row)(col).toString else " ")
        if (col == width - 1) {
          str.append("\n")
        }
      }
      str.toString()
    }

    def count(num:Int):Int = flatData.count(_ == num)

    def numOnesAndTwos():Int = count(1) * count(2)
  }

}
