package day14

import scala.collection.mutable

object Day14 {

  var inventory:mutable.Map[String, Ingredient] = mutable.Map[String, Ingredient]()

  def main(args: Array[String]): Unit = {
    println("Day 14")

    val fuel = Recipe("FUEL", List(Ingredient("A", 7), Ingredient("E", 1)))
    val e = Recipe("E", List(Ingredient("A", 7), Ingredient("D", 1)))
    val b = Recipe("B", List(Ingredient("ORE", 1)))

    inventory.put("ORE", Ingredient("ORE", 10))

    printInventory()
    println (fuel)
    println(e)
    println(b)

    make(b)
  }

  def make(recipe:Recipe):Unit = {
    println("Lets make a(n) " + recipe.name)
    if (checkForIngredients(recipe)) {
      println("Well, we have enough of the right ingredients. So lets do it!")
    }
    useIngredients(recipe)
    val newIngredient = Ingredient(recipe.name, 1)
    inventory.put(recipe.name, newIngredient)

    println("It's made! Here's your inventory now...")
    printInventory()
  }

  def useIngredients(recipe: Recipe):Unit = {
    for (i <- recipe.ingredients) {
      val ingredient = inventory(i.name)
      val updated = Ingredient(i.name, ingredient.amount - i.amount)
      inventory(i.name) = updated
    }
  }

  def checkForIngredients(recipe:Recipe):Boolean = {
    var canMake = true
    for (i <- recipe.ingredients) {
      if (inventory.contains(i.name)) {
        if (i.amount > inventory(i.name).amount) {
          println ("You don't have enough " + i.name + ". You have " + inventory(i.name).amount +
                  " but you need " + i.amount)
          canMake = false
        }
      } else {
        println ("You don't even have any " + i.name + ". You need " + i.amount)
        canMake = false
      }
    }
    canMake
  }

  def printInventory(): Unit = {
    println("You have " + inventory.size + " type(s) of ingredients in your inventory:")
    for (i <- inventory.keys) {
      println("  " + inventory(i))
    }
  }

}

case class Recipe(name:String, ingredients:List[Ingredient]) {

  override def toString: String = {
    val str = new StringBuilder("To Make a(n) " + name + " you need the following ingredients:\n")

    for (i <- ingredients) {
      if (i.amount > 1) {
        str.append("  " + i.amount + " " + i.name + "s\n")
      } else {
        str.append("  " + i.amount + " " + i.name + "\n")
      }
    }

    str.toString()
  }
}

case class Ingredient(name:String, amount:Int)