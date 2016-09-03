// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * The base class for all types of Inventories.
  * Inventories are used to contain ("stow") Equipment.<br>
  * <br>
  * Inventories consist of an internalized grid region - a two-dimensional array - which causes objects to be stored in exclusive positions.
  * @param width the width
  * @param height the height
  */
abstract class Inventory(var width : Int, var height : Int) {
  /**
    * The grid is treated as zero-indexed and its shape two-dimensional.
    * Each cell in the grid, however, is countable from a singular index that is externally significant.
    * Override this property when it needs to be modified for a different implementation of an inventory.
    * Index 134, hex"86", represents the first cell of an Infantry backpack.
    */
  val indexOffset : Int = 134

  /**
    * Get the number of unique items stored in the inventory.
    * Override to account for the datastructure that holds the equipment.
    * @return the count
    */
  def size : Int

  /**
    * Get the full size of the inventory's spatial grid.
    * @return the area, in cells
    */
  def capacity : Int = width * height

  /**
    * Stow an item in the inventory at a set location.<br>
    * <br>
    * The main purpose of this method is to transform an offset singular index into a zero-based two-dimensional index more befitting a grid.
    * Execution is passed to the appropriate method upon successful conversion.
    * @param item the equipment being stowed
    * @param hex an offset singular index that indicates a spatial grid cell
    * @return a Tuple containing (1) whether the stowing succeeded, and (2) what equipment was removed, if any
    */
  def addItem(item : Equipment, hex : Int) : (Boolean, Option[Equipment]) = {
    val coords = Inventory.fromHexIndex(this, hex)
    if(coords._1 != -1) {
      return addItem(item, coords._1, coords._2)
    }
    (false, None)
  }

  /**
    * Stow an item in the inventory at a set location.
    * @param item the equipment
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return a Tuple containing (1) whether the equipment was added and (2) what equipment was removed, if any
    */
  def addItem(item : Equipment, y : Int, x : Int) : (Boolean, Option[Equipment])

  /**
    * Retrieve an item from the inventory based on the item's unique identifier.
    * @param guid the globally unique identifier
    * @return the equipment that was found, if any
    */
  def getItem(guid : Int) : Option[Equipment]

  /**
    * Retrieve an item from the inventory if it is in this inventory.
    * @param item the equipment
    * @return the equipment that was found, if any
    */
  def getItem(item : Equipment) : Option[Equipment]

  /**
    * Retrieve whatever item can be found in the expected spatial grid location in the inventory.
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return the equipment that was found, if any
    */
  def getItem(y : Int, x : Int) : Option[Equipment]

  /**
    * Remove an item from the inventory based on the item's unique identifier.
    * @param guid the globally unique identifier
    * @return the equipment that was found, if any
    */
  def removeItem(guid : Int) : Option[Equipment]

  /**
    * Remove an item from the inventory if it is in this inventory.
    * @param item the equipment
    * @return the equipment that was removed, if any
    */
  def removeItem(item : Equipment) : Option[Equipment]

  /**
    * Remove whatever item can be found in the expected grid location in the inventory.
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return the equipment that was removed, if any
    */
  def removeItem(y : Int, x : Int) : Option[Equipment]

  /**
    * Resize the dimensions of this inventory.
    * All contents of the inventory are dropped.
    * @param w the new width of the inventory
    * @param h the new height of the inventory
    * @return a List of all Equipment that was previously contained in the Inventory
    */
  def resize(w : Int, h : Int) : List[Equipment]
}

object Inventory {
  /**
    * Convert a two-dimensional index into an offset singular index respective to the given Inventory.
    * @param context the instance of the Inventory for which this coordinate is being converted
    * @param y the y-coordinate of the original index
    * @param x the x-coordinate of the original index
    * @return the index value, or an invalid -1
    */
  def toHexIndex(context : Inventory, y : Int, x : Int) : Int = {
    val width : Int = context.width
    val height : Int = context.height
    if(y < 0 || x < 0 || y >= height || x >= width)
      return -1 // No negative coordinates

    val index : Int = y * width + x
    if(index >= width * height) -1 else context.indexOffset + index // Our index was too high?
  }

  /**
    * Convert an singular index into a zero-based two-dimensional index respective to the given Inventory.
    * @param context the instance of the Inventory for which this coordinate is being converted
    * @param hex the original index
    * @return a Tuple containing (1) the y-coordinate and (2) the x-coordinate, or an invalid (-1, -1)
    */
  def fromHexIndex(context : Inventory, hex : Int) : (Int, Int) = {
    val offset : Int = context.indexOffset
    if(hex < offset)
      return (-1, -1) // If we're less than the offset, we can not be in this inventory

    val width : Int = context.width
    val value : Int = hex - offset
    val y : Int = math.floor(value / width).toInt
    if(y >= context.height)
      return (-1, -1) // Our index was too high
    val x : Int = value - (y * width)
    (y, x)
  }
}
