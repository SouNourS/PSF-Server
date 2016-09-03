// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.annotation.switch
import scala.util.control.Breaks._
import scala.collection.mutable

/**
  * na
  *
  * @param width
  * @param height
  */
class GridInventory(w : Int, h : Int) extends Inventory(w, h) {
  protected var contents : mutable.HashMap[Int,InventoryItem] = new mutable.HashMap[Int,InventoryItem]
  protected var grid : Array[Int] = _
  resize(width, height) // Call to setup storage space, as need be

  def size : Int = {
    contents.size
  }

  /**
    * Insert an item into the grid inventory at a set location.<br>
    * <br>
    * Each insertion makes "inventory area" number of checks, and performs "inventory area" set operations if that passes.
    * In this way, each insertion costs twice the "inventory area" of the item on insertion for each item.
    * The meaning behind this is that there is no upkeep cost for performing the insertion of multiple objects.
    * Insertion complexity is `O(2n)`.<br>
    * <br>
    * Assuming two inventories - the 6x6 of Infiltration and the 9x12 of Reinforced - we will fill these inventories up with items.
    * First, we will fill them with 3x3 items; second, we will fill them with 2x2 items.
    * In the first case, the 6x6 inventory fills with 4 items in 72 passes.
    * The 9x12 inventory takes 12 items in 72 passes as well.
    * In the second case, the 6x6 inventory fills with 9 items in 216 passes.
    * The 9x12 inventory takes 24 items in 192 passes.
    * @param item
    * @param x
    * @param y
    * @return
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
        setRange(x, y, nsize._2, nsize._1, item.guid)

      case 1 =>
        val swapopt = contents.remove(overlap.head).get
        swap = Option(swapopt.obj)
        if(swap.isDefined) {
          val isize = swap.get.getInventorySize
          setRange(swapopt.x, swapopt.y, isize._2, isize._1)
        }
        contents += (item.guid -> InventoryItem(item, y, x))
        val nsize = item.getInventorySize
        setRange(x, y, nsize._2, nsize._1, item.guid)

      case _ =>
        success = false
    }
    (success, swap)
  }

  private def testForOverlap(item : Equipment, y : Int, x : Int) : List[Int] = {
    val w : Int = item.getInventorySize._2
    val h : Int = item.getInventorySize._1
    if(w < 0 || h < 0 || w < x || h < y || x+w >= width || y+h >= height)
      return Nil

    var list: mutable.HashMap[Int,Boolean] = new mutable.HashMap[Int,Boolean]

    for(dy <- 0 to h) {
      val row0 : Int = (y + dy) * width + x
      for(dx <- 0 to w) {
        val index : Int = grid(row0 + dx)
        if(index > -1)
            list += (index -> true)
      }
    }
    list.keySet.toList
  }

  private def setRange(x : Int, y : Int, w : Int, h : Int, value : Int = -1) : Boolean = {
    if(w < 0 || h < 0 || w < x || h < y || x+w >= width || y+h >= height)
      return false

    for(dy <- 0 to h) {
      val row0 : Int = (y + dy) * width + x
      for(dx <- 0 to w) {
        val index : Int = grid(row0 + dx)
        grid(index) = value
      }
    }
    true
  }

  def getItem(guid : Int) : Option[Equipment] = {
    val itemopt = contents.get(guid)
    if(itemopt.isDefined)
      return Option(itemopt.get.obj)
    None
  }

  def getItem(item : Equipment) : Option[Equipment] = {
    contents.foreach({
      case (key : Int, value : InventoryItem) =>
        if(value.obj eq item)
          return Option(value.obj)
    })
    None
  }

  def getItem(y : Int, x : Int) : Option[Equipment] = {
    val index : Int = y * width + x
    if(0 <= index && index < height * width) {
      return getItem(grid(index))
    }
    None
  }

  def removeItem(guid : Int) : Option[Equipment] = {
    var removed : Option[Equipment] = None
    val removopt : Option[InventoryItem] = contents.remove(guid)
    if(removopt.isDefined) {
      val invItem : InventoryItem = removopt.get
      val obj = invItem.obj
      setRange(invItem.x, invItem.y, obj.getInventorySize._2, obj.getInventorySize._1)
      removed = Option(obj)
    }
    removed
  }

  def removeItem(item : Equipment) : Option[Equipment] = {
    var removed : Option[Equipment] = removeItem(item.guid)
    if(removed.isDefined)
      return removed

    contents.foreach { case (key: Int, stowed: InventoryItem) =>
      if(stowed.obj eq item) {
        //TODO we found the correct equipment in the inventory under a different guid; what happened?
        return removeItem(key)
      }
    }
    None
  }

  def removeItem(y : Int, x : Int) : Option[Equipment] = {
    var removed : Option[Equipment] = None

    if(x >= 0 || x < width || y > 0 || y < height) {
      val guid: Int = grid(y * width + x)
      if(guid > -1) {
        removed = removeItem(guid)
        break
      }
    }
    removed
  }

  def resize(w : Int, h : Int) : List[Equipment] = {
    if(w < 0 || h < 0)
      Nil
    width = w
    height = h
    grid = Array.fill[Int](w * h)(-1)

    if(contents.nonEmpty) {
      var temp : List[InventoryItem] = contents.values.toList
      var dropped : mutable.ListBuffer[Equipment] = new mutable.ListBuffer[Equipment]
      contents.clear
      //TODO space-fitting algorithm to push equipment back into inventory within its new constraints
      //TODO elements that don't fit will get dropped
      return dropped.toList
    }
    Nil
  }

  override def toString : String = {
    GridInventory.toString(this)
  }
}

object GridInventory {
  def apply(w : Int, h : Int) : GridInventory = {
    new GridInventory(w, h)
  }

  def toString(obj : GridInventory) : String = {
    "[inventory: %dx%d, %d items]".format(obj.width, obj.height, obj.contents.size)
  }
}
