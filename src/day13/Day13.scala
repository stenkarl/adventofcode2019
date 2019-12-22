package day13

import util.File

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Day13 {

  val program = File.fromFile("src/day13/day13.txt").head.split(',').map(s => BigInt(s.toString))

  var curX:Int = -2
  var curY:Int = -2
  var width:Int = 0
  var height:Int = 0
  var curScore:Int = 0

  var scoreMode = false

  val blocks = ListBuffer[Point]()
  var paddle = Point(0,0)
  var ball = Point(0,0)

  val recordedInput = File.fromFile("src/day13/moves.txt").head.split(',').map(s => s.trim().toInt)
  val moves = ListBuffer[Int]()
  var turn = 0
  var brokenBlocks = 0

  def main(args: Array[String]): Unit = {
    new IntcodeComputer(program, onInput, onOutput).run()

    //println(moves)
  }

  def onInput():Int = {
    if (paddle.x < ball.x) 1 else if (paddle.x > ball.x) -1 else 0
  }

  def onInput1():Int = {
    display()
    if (turn < recordedInput.size) {
      val move = recordedInput(turn)
      turn += 1
      moves += move
      return move
    }
    print("input> ")
    val move = StdIn.readInt()
    moves += move
    move
  }

  def onOutput(output:Int):Unit = {
    //println(output)
    if (curX == -2) {
      curX = output
      if (curX == -1){
        scoreMode = true
      }
      if (curX > width) {
        width = curX
      }
    } else if (curY == -2) {
      curY = output
      if (curY > height) {
        height = curY
      }
    } else {
      if (scoreMode) {
        scoreMode = false
        curScore = output
        display()
      } else if (output == 2) {
        blocks += Point(curX, curY)
      } else if (output == 0) {0
        val p = Point(curX, curY)
        if (blocks.contains(p)) {
          blocks -= p
          brokenBlocks += 1
        }
      } else if (output == 4) {
        ball = Point(curX, curY)
      } else if (output == 3) {
        paddle = Point(curX, curY)
      }
      curX = -2
      curY = -2
    }
  }

  def display(): Unit = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val p = Point(x, y)
        if (blocks.contains(p)) {
          print("X")
        } else if (ball == p) {
          print("O")
        } else if (paddle == p) {
          print("=")
        } else {
          print(" ")
        }
      }
      println()
    }
    println("0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L")
    println("Score: " + curScore + ", Blocks Broken: " + brokenBlocks + ", Turn: " + turn)
  }

}

case class Point(x:Int, y:Int)


