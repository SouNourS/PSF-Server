// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.annotation.switch
import scala.collection.mutable

/**
  * An implementation of an Inventory that constructs a concrete grid to visualize the locations or equipment stowed in it.
  * @param w the width
  * @param h the height
  */
class GridInventory(w : Int, h : Int) extends Inventory(w, h) {
  /**
    * Even though it's a "Grid" inventory, the internal storage is handled by a mutable hashmap.
    * (It __was__ a List at one point.)
    * The keys are equipment GUIDs.
    * The values are an intermediary structure designed to hold the cell coordinates of the top-left of the inventory item tile.
    */
  protected var contents : mutable.HashMap[Int,InventoryItem] = new mutable.HashMap[Int,InventoryItem]
  /**
    * The data structure of the concrete grid is an Array for storing equipment GUIDs.
    * The length of the Array is `width` times `height`.
    * Each row is `width` cells long.
    * The default value for the cells is `-1`.
    * When an insertion occurs, the cells that would correspond to where the item is stowed are set to teh item's GUID.
    */
  protected var grid : Array[Int] = _
  resize(width, height) // Call to set up grid Array

  def size : Int = {
    contents.size
  }

  /**
    * Insert an item into the grid inventory at a set location.<br>
    * <br>
    * Each piece of equipment has an "inventory area."
    * Each insertion causes a number of checks equal to the size of this "inventory area" and performs just as many set operations to record the GUID.
    * The cost of any insertion is `O(2m)` for the area of the equipments' tiles.
    * @param item the equipment being stowed
    * @param x the y-coordinate of the location
    * @param y the x-coordinate of the location
    * @return a Tuple containing (1) whether the equipment was added and (2) what equipment was removed, if any
    */
  def addItem(item : Equipment, y : Int, x : Int) : (Boolean, Option[Equipment]) = {
    if(Option(item).isEmpty || x < 0 || y < 0 || x + item.getInventorySize._1 > width || y + item.getInventorySize._2 > height)
      return (false, None)
    else if(contents.get(item.guid).isDefined) {
      val obj = contents(item.guid).obj
      if(obj ne item) {
        //TODO actually a really concerning issue!
      }
      return (false, None)
    }

    val overlap : List[Int] = testForOverlap(item, y, x)
    var success = true
    var swap : Option[Equipment] = None
    (overlap.size: @switch) match {
      case 0 =>
        contents += (item.guid -> InventoryItem(item, y, x))
        val nsize = item.getInventorySize
        setCellsToValue(x, y, nsize._1, nsize._2, item.guid)

      case 1 =>
        val swapopt = contents.remove(overlap.head).get
        swap = Option(swapopt.obj)
        if(swap.isDefined) {
          val isize = swap.get.getInventorySize
          setCellsToValue(swapopt.x, swapopt.y, isize._1, isize._2)
        }
        contents += (item.guid -> InventoryItem(item, y, x))
        val nsize = item.getInventorySize
        setCellsToValue(x, y, nsize._1, nsize._2, item.guid)

      case _ =>
        success = false
    }
    (success, swap)
  }

  /**
    * Test for equipment overlap in the two-dimensional region of the inventory.
    * Recover all items in the Inventory that we would be overlapping.<br>
    * <br>
    * The cost is `O(m)` for the area of the equipments' tiles.
    * @param item the equipment being stowed
    * @param x the y-coordinate of the location
    * @param y the x-coordinate of the location
    * @return a List of equipment GUIDs that the item would overlap
    */
  protected def testForOverlap(item : Equipment, y : Int, x : Int) : List[Int] = {
    val w : Int = item.getInventorySize._1
    val h : Int = item.getInventorySize._2
    if(x < 0 || y < 0 || w < 0 || h < 0 || x+w >= width || y+h >= height)
      return Nil

    var list: mutable.ListBuffer[Int] = new mutable.ListBuffer[Int]
    for(dy <- 0 until h) {
      val row0 : Int = (y + dy) * width + x
      for(dx <- 0 until w) {
        val index : Int = grid(row0 + dx)
        if(index > -1)
            list += index
      }
    }
    list.toList
  }

  /**
    * Change values in the grid data structure to a prescribed value.
    * The value is typically a GUID.<br>
    * <br>
    * The cost is `O(m)` for the area of the equipments' tiles.
    * @param x the x-coordinate that is "left"
    * @param y the y-coordinate that is "top"
    * @param w the width
    * @param h the height
    * @param value the GUID to change the affected cells, defaults to -1
    */
  protected def setCellsToValue(x : Int, y : Int, w : Int, h : Int, value : Int = -1) : Unit = {
    for(dy <- 0 until h) {
      val row0 : Int = (y + dy) * width + x
      for(dx <- 0 until w) {
        grid(row0 + dx) = value
      }
    }
  }

  /**
    * Retrieve an item from the inventory based on the item's unique identifier.<br>
    * <br>
    * The cost is `O(1)`.
    * @param guid the globally unique identifier
    * @return the equipment that was found, if any
    */
  def getItem(guid : Int) : Option[Equipment] = {
    val itemopt = contents.get(guid)
    if(itemopt.isDefined)
      return Option(itemopt.get.obj)
    None
  }

  /**
    * Retrieve an item from the inventory if it is in this inventory.<br>
    * <br>
    * Checks for object equivalence, so all of the elements that have been stowed are checked.
    * Best case cost is `O(1)` and worst case is `O(n)` for the number of elements.
    * @param item the equipment
    * @return the equipment that was found, if any
    */
  def getItem(item : Equipment) : Option[Equipment] = {
    contents.foreach({
      case (key : Int, value : InventoryItem) =>
        if(value.obj eq item)
          return Option(value.obj)
    })
    None
  }

  /**
    * Retrieve whatever item can be found in the expected location in the inventory.<br>
    * <br>
    * The cost is `O(1)`.
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return the equipment that was found, if any
    */
  def getItem(y : Int, x : Int) : Option[Equipment] = {
    val index : Int = y * width + x
    if(0 <= index && index < height * width) {
      return getItem(grid(index))
    }
    None
  }

  /**
    * Remove an item from the inventory based on the item's unique identifier.<br>
    * <br>
    * The cost is `O(m)` for the area of the equipment's tile.
    * @param guid the globally unique identifier
    * @return the equipment that was found, if any
    */
  def removeItem(guid : Int) : Option[Equipment] = {
    val removopt : Option[InventoryItem] = contents.remove(guid)
    if(removopt.isDefined) {
      val invItem : InventoryItem = removopt.get
      val obj = invItem.obj
      setCellsToValue(invItem.x, invItem.y, obj.getInventorySize._1, obj.getInventorySize._2)
      return Option(obj)
    }
    None
  }

  /**
    * Remove an item from the inventory if it is in this inventory.
    * Checks for object equivalence, so all of the elements that have been stowed are checked.<br>
    * <br>
    * Best case cost is `O(m)` and worst case is `O(n x m)` for `n` the number of elements and `m` the area of the equipments' tiles.
    * @param item the equipment
    * @return the equipment that was removed, if any
    */
  def removeItem(item : Equipment) : Option[Equipment] = {
    contents.foreach { case (key: Int, stowed: InventoryItem) =>
      if(stowed.obj eq item) {
        return removeItem(key)
      }
    }
    None
  }

  /**
    * Remove whatever item can be found in the expected grid location in the inventory.<br>
    * <br>
    * The cost is `O(m)` for the area of the equipment's tile.
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return the equipment that was removed, if any
    */
  def removeItem(y : Int, x : Int) : Option[Equipment] = {
    if(x >= 0 || x < width || y > 0 || y < height) {
      val guid : Int = grid(y * width + x)
      if(guid > -1) {
        return removeItem(guid)
      }
    }
    None
  }

  /**
    * Resize the dimensions of this inventory.
    * All contents of the inventory are dropped and their previous locations in it are discarded.
    * The grid data structure is completely reconstructed.<br>
    * <br>
    * The cost is always `O(n)` for the number of elements.
    * @param w the new width of the inventory
    * @param h the new height of the inventory
    * @return a List of all Equipment that was previously contained in the Inventory
    */
  override def resize(w : Int, h : Int) : List[Equipment] = {
    super.resize(w, h)
    grid = Array.fill[Int](w * h)(-1)

    if(contents.nonEmpty) {
      var dropped : mutable.ListBuffer[Equipment] = new mutable.ListBuffer[Equipment]
      contents.values.foreach({ value : InventoryItem => dropped += value.obj })
      contents.clear
      return dropped.toList
    }
    Nil
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    GridInventory.toString(this)
  }
}

object GridInventory {
  /**
    * A constructor that accepts the minimum parameters.
    * @param w the width
    * @param h the height
    * @return the GridInventory
    */
  def apply(w : Int, h : Int) : GridInventory = {
    new GridInventory(w, h)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : GridInventory) : String = {
    "{inventory(%dx%d): %d items}".format(obj.width, obj.height, obj.size)
  }
}
