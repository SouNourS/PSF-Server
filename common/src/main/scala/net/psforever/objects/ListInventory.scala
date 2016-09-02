// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.annotation.switch
import scala.util.control.Breaks._
import scala.collection.mutable

class ListInventory(w : Int, h : Int) extends Inventory(w, h) {
  protected var contents : mutable.HashMap[Int, InventoryItem] = new mutable.HashMap[Int, InventoryItem]
  resize(width, height) // Call to setup storage space, as need be

  def size : Int = {
    contents.size
  }

  /**
    * Insert an item into the inventory at a set location.<br>
    * <br>
    * Each insertion makes "size of contents" number of checks, and performs a set operation if that passes.
    * The first insertion is, therefore, free.
    * Each subsequent insertion adds on the previous insertion elements as a cost to the current one.
    * The given insertion of an nth element costs `T(n) = n - 1 + T(n-1), T(1) = 0`.
    * Earlier insertions are cheaper than later insertions; and, the later the insertion, the more expensive it is.<br>
    * <br>
    * Assuming two inventories - the 6x6 of Infiltration and the 9x12 of Reinforced - we will fill these inventories up with items.
    * The size of the inventory only matters in regards to how many items we can safely insert into it.
    * First, we will fill them with 3x3 items; second, we will fill them with 2x2 items.
    * In the first case, the 6x6 inventory fills with 4 items in 6 passes.
    * The 9x12 inventory takes 12 items in 65 passes.
    * In the second case, the 6x6 inventory fills with 9 items in 36 passes.
    * The 9x12 inventory takes 24 items in 276 passes.
    * @param item
    * @param x
    * @param y
    * @return
    */
  def addItem(item : Equipment, y : Int, x : Int) : (Boolean, Option[Equipment]) = {
    if(x < 0 || y < 0 || x + item.getInventorySize._1 > width || y + item.getInventorySize._2 > height)
      return (false, None)

    val overlap : List[Int] = testForOverlap(item, y, x)
    var success = true
    var swap : Option[Equipment] = None
    (overlap.size: @switch) match {
      case 0 =>
        contents += item.guid -> InventoryItem(item, y, x)
      case 1 =>
        swap = Option(contents.remove(overlap.head).get.obj)
        contents += item.guid -> InventoryItem(item, y, x)
      case _ =>
        success = false
    }
    (success, swap)
  }

  def testForOverlap(item : Equipment, y : Int, x : Int) : List[Int] = {
    val itemx0 = x
    val itemy0 = y
    val itemx1 = x + item.getInventorySize._2
    val itemy1 = y + item.getInventorySize._1

    var list : mutable.ListBuffer[Int] = new mutable.ListBuffer[Int]()
    contents.foreach({ case (key : Int, value : InventoryItem) =>
      val stowx0 = value.x
      val stowy0 = value.y
      val stowx1 = stowx0 + value.obj.getInventorySize._2
      val stowy1 = stowy0 + value.obj.getInventorySize._1
      if((itemx0 >= stowx0 && itemx0 <= stowx1) || (itemx1 >= stowx0 && itemx1 <= stowx1) &&
        (itemy0 >= stowy0 && itemy0 <= stowy1) || (itemy1 >= stowy0 && itemy1 <= stowy1)) {
        list += key
      }
    })
    list.toList
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
    contents.foreach({
      case (key : Int, value : InventoryItem) =>
        val h : Int = value.y + value.obj.getInventorySize._1
        val w : Int = value.x + value.obj.getInventorySize._2
        if(0 <= x && x < w && 0 <= y && y < h)
          return Option(value.obj)
    })
    None
  }

  def removeItem(guid : Int) : Option[Equipment] = {
    var removed : Option[Equipment] = None
    val removopt : Option[InventoryItem] = contents.remove(guid)
    if(removopt.isDefined)
      removed = Option(removopt.get.obj)
    removed
  }

  def removeItem(item : Equipment) : Option[Equipment] = {
    var removed : Option[Equipment] = removeItem(item.guid)
    if(removed.isDefined)
      return removed

    contents.foreach({ case (key: Int, stowed: InventoryItem) =>
      if(stowed.obj eq item) {
        //TODO we found the correct equipment in the inventory under a different guid; what happened?
        removed = Option(contents.remove(key).get.obj)
        break
      }
    })
    removed
  }

  def removeItem(x : Int, y : Int) : Option[Equipment] = {
    var removed : Option[Equipment] = None

    if(x >= 0 || x < width || y > 0 || y < height) {
      contents.foreach({ case (key: Int, stowed: InventoryItem) =>
        val stowx0 = stowed.x
        val stowy0 = stowed.y
        val stowx1 = stowx0 + stowed.obj.getInventorySize._2
        val stowy1 = stowy0 + stowed.obj.getInventorySize._1
        if((x >= stowx0 && x <= stowx1) || (y >= stowy0 && y <= stowy1)) {
          removed = Option(contents.remove(key).get.obj)
          break
        }
      })
    }
    removed
  }

  def resize(w : Int, h : Int) : List[Equipment] = {
    if(w < 0 || h < 0)
      Nil
    width = w
    height = h

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
    ListInventory.toString(this)
  }
}

object ListInventory {
  def apply(w : Int, h : Int) : ListInventory = {
    new ListInventory(w, h)
  }

  def toString(obj : ListInventory) : String = {
    "[inventory: %dx%d, %d items]".format(obj.width, obj.height, obj.contents.size)
  }
}
