package day12

import scala.collection.mutable.ListBuffer

object Day12 {

  //val moons = List[Moon](Moon(-1, 0, 2), Moon(2, -10, -7), Moon(4, -8, 8), Moon(3, 5, -1))
  val moons = List[Moon](Moon(5, -1, 5), Moon(0, -14, 2), Moon(16, 4, 0), Moon(18, 1, 16))
  val states = ListBuffer[List[Moon]]()
  var steps = BigInt(0)

  var found = false

  def main(args: Array[String]): Unit = {
    while (!found) {
      step()
    }

    //println(moons)
    //println("Total Energy " + totalEnergy)
  }

  def step() = {
    applyGravity()
    updatePositions()
    saveState()
    steps += 1
  }

  def saveState() = {
    val state = List[Moon](Moon(moons(0).position.x, moons(0).position.y, moons(0).position.z),
                          Moon(moons(1).position.x, moons(1).position.y, moons(1).position.z),
                          Moon(moons(2).position.x, moons(2).position.y, moons(2).position.z),
      Moon(moons(3).position.x, moons(3).position.y, moons(3).position.z)
    )

    state(0).velocity = moons(0).velocity
    state(1).velocity = moons(1).velocity
    state(2).velocity = moons(2).velocity
    state(3).velocity = moons(3).velocity

    if (states.contains(state)) {
      println("Found repeat state after " + steps + " steps")
      found = true
      //System.exit(0)
    }
    states += state


  }

  def totalEnergy = moons.foldLeft(0)(_ + _.energy())

  def applyGravity(): Unit = {
    val combos = moons.combinations(2).toList

    //println(combos)
    for (combo <- combos) {
      val m1 = combo(0)
      val m2 = combo(1)

      val x = pull(m1.position.x, m2.position.x)
      val y = pull(m1.position.y, m2.position.y)
      val z = pull(m1.position.z, m2.position.z)

      val m1Vel = Velocity(m1.velocity.x + x, m1.velocity.y + y, m1.velocity.z + z)
      m1.velocity = m1Vel

      val m2Vel = Velocity(m2.velocity.x - x, m2.velocity.y - y, m2.velocity.z - z)
      m2.velocity = m2Vel

    }
  }

  def updatePositions():Unit = {
    for (m <- moons) {
      val pos = Position(m.position.x + m.velocity.x, m.position.y + m.velocity.y, m.position.z + m.velocity.z)
      m.position = pos
    }
  }

  def pull(first:Int, second:Int):Int = if (first > second) -1 else if (first < second) 1 else 0

}

case class Moon(initX:Int, initY:Int, initZ:Int) {

  var position = Position(initX, initY, initZ)
  var velocity = Velocity(0, 0, 0)

  override def toString: String = "Moon[position:" + position + ", velocity:" + velocity + "]"

  def energy():Int = {
    val pot = Math.abs(position.x) + Math.abs(position.y) + Math.abs(position.z)
    val kin = Math.abs(velocity.x) + Math.abs(velocity.y) + Math.abs(velocity.z)

    pot * kin
  }

  override def equals(obj: Any): Boolean = {
    val m = obj.asInstanceOf[Moon]

    this.position.x == m.position.x && this.velocity.x == m.velocity.x
  }

}

case class Velocity(x:Int, y:Int, z:Int)

case class Position(x:Int, y:Int, z:Int)

