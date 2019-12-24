package day14

import util.File

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day14 {

  val recipePath = "src/day14/day14.txt"

  var inventory:mutable.Map[String, Ingredient] = mutable.Map[String, Ingredient]()
  var used:mutable.Map[String, Ingredient] = mutable.Map[String, Ingredient]()

  val recipes = ListBuffer[Recipe]()

  def main(args: Array[String]): Unit = {
    println("Day 14")

    inventory.put("ORE", Ingredient("ORE", 1000000000))

    printInventory()

    createRecipes()
    println(recipes)

    //shop(lookupRecipe("FUEL"))
    make(lookupRecipe("FUEL"))
    printUsed()
  }

  def lookupRecipe(name:String):Recipe = {
    val recipe = recipes.find(_.name.name == name)
    if (!recipe.isDefined) {
      println("ERROR: lookupRecipe failed for " + name)
      System.exit(0)
    }
    recipe.get
  }

  def shop(recipe: Recipe):Unit = {
    println("shop: " + recipe.name)
    for (i <- recipe.ingredients) {
      val recipe = recipes.find(_.name.name == i.name)
      if (recipe.isDefined) {
        shop(recipe.get)
      } else {
        println("Basic Ingredient " + i)
      }
    }
  }

  def createRecipes(): Unit = {
    val file = File.fromFile(recipePath)

    file.foreach { line =>
      val parts = line.split(" => ")
      val creates = ingredientFromString(parts(1))
      val ingredients:List[Ingredient] = parts(0).split(", ").map(ingredientFromString).toList

      recipes += Recipe(creates, ingredients)
    }
  }

  def ingredientFromString(str:String):Ingredient = {
    val parts = str.split(" ")
    Ingredient(parts(1), parts(0).toInt)
  }

  def make(recipe:Recipe):Unit = {
    println("Lets make " + recipe.name)
    checkForIngredients(recipe)
    println("Well, we have enough of the right ingredients to make " + recipe.name + ". So lets do it!")
    useIngredients(recipe)
    addToInventory(recipe.name)

    println("Made " + recipe.name + ". Here's your inventory now...")
    printInventory()
    printUsed()
  }

  def addToInventory(i:Ingredient):Unit = {
    if (inventory.contains(i.name)) {
      val updated = Ingredient(i.name, inventory(i.name).amount + i.amount)
      inventory(i.name) = updated
    } else {
      inventory(i.name) = i
    }
  }

  def useIngredients(recipe: Recipe):Unit = {
    for (i <- recipe.ingredients) {
      while (!hasEnough(i)) {
        make(lookupRecipe(i.name))
      }
      val ingredient = inventory(i.name)
      val updated = Ingredient(i.name, ingredient.amount - i.amount)
      if (updated.amount > 0) {
        inventory(i.name) = updated
      } else if (updated.amount == 0) {
        inventory.remove(i.name)
      } else {
        println("ERROR: Negative ingredients: " + updated)
      }
      updateUsed(i)
    }
  }

  def updateUsed(i:Ingredient):Unit = {
    if (used.contains(i.name)) {
      val updated = Ingredient(i.name, used(i.name).amount + i.amount)
      used(i.name) = updated
    } else {
      used(i.name) = i
    }
  }

  def checkForIngredients(recipe:Recipe):Unit = {
    for (i <- recipe.ingredients) {
      println("Checking for " + i)
      while (!hasEnough(i)) {
        make(lookupRecipe(i.name))
      }
    }
  }

  def hasEnough(i:Ingredient):Boolean = {
    if (inventory.contains(i.name)) {
      if (i.amount > inventory(i.name).amount) {
        println ("You don't have enough " + i.name + ". You have " + inventory(i.name).amount +
          " but you need " + i.amount)
        return false
      }
    } else {
      println ("You don't even have any " + i.name + ". You need " + i.amount)
      return false
    }
    println("We have enough " + i)
    true
  }

  def printInventory(): Unit = {
    println("You have " + inventory.size + " type(s) of ingredients in your inventory:")
    for (i <- inventory.keys) {
      println("  " + inventory(i))
    }
  }

  def printUsed(): Unit = {
    println("You used " + used.size + " type(s) of ingredients in your making of things:")
    for (i <- used.keys) {
      println("  " + used(i))
    }
  }

}

case class Recipe(name:Ingredient, ingredients:List[Ingredient]) {

  override def toString: String = {
    val str = new StringBuilder("Recipe: To Make " + name.amount + " " + name.name +
                                " you need the following: ")

    str.append(ingredients.mkString(", "))

    str.toString()
  }
}

case class Ingredient(name:String, amount:Int) {

  override def toString: String = amount + " " + name + (if (amount > 1) "s" else "")
}