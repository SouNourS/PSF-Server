// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.annotation.switch
import scala.collection.mutable

/**
  * The basic implementation of an Inventory.
  * @param w the width
  * @param h the height
  */
class ListInventory(w : Int, h : Int) extends Inventory(w, h) {
  /**
    * Even though it's a "List" inventory, the internal storage is handled by a mutable hashmap.
    * (It __was__ a List at one point.)
    * The keys are equipment GUIDs.
    * The values are an intermediary structure designed to hold the cell coordinates of the top-left of the inventory item tile.
    */
  protected var contents : mutable.HashMap[Int, InventoryItem] = new mutable.HashMap[Int, InventoryItem]

  def size : Int = {
    contents.size
  }

  /**
    * Stow an item in the inventory at a set location.
    * This set location is defined by two coordinates for the top-left corner.<br>
    * <br>
    * Each insertion makes "size of contents" number of checks and performs a set operation if that passes.
    * The first insertion is, therefore, free.
    * Each subsequent insertion adds on the previous insertion elements as a cost to the current one.
    * The given insertion of an nth element costs `T(n) = n - 1 + T(n-1), T(1) = 0`.
    * Earlier insertions are cheaper than later insertions.
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

    var success = true
    val overlap : List[Int] = testForOverlap(item, y, x)
    var swap : Option[Equipment] = None
    (overlap.size: @switch) match {
      case 0 => // No overlaps: just add
        contents += item.guid -> InventoryItem(item, y, x)
      case 1 => // One overlap: remove the overlapped item and add the new item
        swap = Option(contents.remove(overlap.head).get.obj)
        contents += item.guid -> InventoryItem(item, y, x)
      case _ => // Too many overlaps: do nothing
        success = false
    }
    (success, swap)
  }

  /**
    * Test for equipment overlap in the two-dimensional region of the inventory.
    * Recover all items in the Inventory that we would be overlapping.<br>
    * <br>
    * The cost is always `O(n)` for the number of elements.
    * @param item the equipment being stowed
    * @param x the y-coordinate of the location
    * @param y the x-coordinate of the location
    * @return a List of equipment GUIDs that the item would overlap
    */
  protected def testForOverlap(item : Equipment, y : Int, x : Int) : List[Int] = {
    val w = x + item.getInventorySize._1 //item
    val h = y + item.getInventorySize._2

    var list : mutable.ListBuffer[Int] = new mutable.ListBuffer[Int]()
    contents.foreach({ case (key : Int, value : InventoryItem) =>
      val sx = value.x //test
      val sy = value.y
      val sw = sx + value.obj.getInventorySize._1
      val sh = sy + value.obj.getInventorySize._2
      if( ((sx <= x && x < sw) || (sx < w && w < sw) || (x <= sx && sx < w)) &&
        ((sy <= y && y < sh) || (sy < h && h < sh) || (y <= sy && sy < h)) ) {
        // The third cases check if the existing item is embedded in the new item within that coordinate dimension
        list += key
      }
    })
    list.toList
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
    * Retrieve an item from the inventory if it is in this inventory.
    * Checks for object equivalence, so all of the elements that have been stowed are checked.<br>
    * <br>
    * Best case cost is `O(1)` and worst case is `O(n)` for the number of elements.
    * @param item the equipment
    * @return the equipment that was found, if any
    */
  def getItem(item : Equipment) : Option[Equipment] = {
    if(Option(item).isDefined) {
      contents.foreach({
        case (_, value : InventoryItem) =>
          if(value.obj eq item)
            return Option(value.obj)
      })
    }
    None
  }

  /**
    * Retrieve whatever item can be found in the expected location in the inventory.<br>
    * <br>
    * Best case cost is `O(1)` and worst case is `O(n)` for the number of elements.
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return the equipment that was found, if any
    */
  def getItem(y : Int, x : Int) : Option[Equipment] = {
    if(x >= 0 && x < width && y >= 0 && y < height) {
      contents.foreach({
        case (key : Int, value : InventoryItem) =>
          val sx : Int = value.x
          val sy : Int = value.y
          val sw : Int = sx + value.obj.getInventorySize._1
          val sh : Int = sy + value.obj.getInventorySize._2
          if(sx <= x && x < sw && sy <= y && y < sh)
            return Option(value.obj)
      })
    }
    None
  }

  /**
    * Remove an item from the inventory based on the item's unique identifier.<br>
    * <br>
    * Best case cost is `O(1)` and worst case is `O(n)` for the number of elements.
    * @param guid the globally unique identifier
    * @return the equipment that was found, if any
    */
  def removeItem(guid : Int) : Option[Equipment] = {
    val removopt : Option[InventoryItem] = contents.remove(guid)
    if(removopt.isDefined)
      return Option(removopt.get.obj)
    None
  }

  /**
    * Remove an item from the inventory if it is in this inventory.
    * Checks for object equivalence, so all of the elements that have been stowed are checked.<br>
    * <br>
    * Best case cost is `O(1)` and worst case is `O(n)` for the number of elements.
    * @param item the equipment
    * @return the equipment that was removed, if any
    */
  def removeItem(item : Equipment) : Option[Equipment] = {
    if(Option(item).isDefined) {
      contents.foreach({
        case (key : Int, value : InventoryItem) =>
          if(value.obj eq item) {
            contents.remove(key)
            return Option(value.obj)
          }
      })
    }
    None
  }

  /**
    * Remove whatever item can be found in the expected grid location in the inventory.<br>
    * <br>
    * Best case cost is `O(1)` and worst case is `O(n)` for the number of elements.
    * @param y the y-coordinate of the location
    * @param x the x-coordinate of the location
    * @return the equipment that was removed, if any
    */
  def removeItem(y : Int, x : Int) : Option[Equipment] = {
    if(x >= 0 && x < width && y >= 0 && y < height) {
      contents.foreach({
        case (key : Int, value : InventoryItem) =>
          val sx : Int = value.x
          val sy : Int = value.y
          val sw : Int = sx + value.obj.getInventorySize._1
          val sh : Int = sy + value.obj.getInventorySize._2
          if(sx <= x && x < sw && sy <= y && y < sh) {
            contents.remove(key)
            return Option(value.obj)
          }
       })
    }
    None
  }

  /**
    * Resize the dimensions of this inventory.
    * All contents of the inventory are dropped and their previous locations in it are discarded.<br>
    * <br>
    * The cost is always `O(n)` for the number of elements.
    * @param w the new width of the inventory
    * @param h the new height of the inventory
    * @return a List of all Equipment that was previously contained in the Inventory
    */
  override def resize(w : Int, h : Int) : List[Equipment] = {
    super.resize(w, h)

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
    ListInventory.toString(this)
  }
}

object ListInventory {
  /**
    * A constructor that accepts the minimum parameters.
    * @param w the width
    * @param h the height
    * @return the ListInventory
    */
  def apply(w : Int, h : Int) : ListInventory = {
    new ListInventory(w, h)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : ListInventory) : String = {
    "{inventory(%dx%d): %d items}".format(obj.width, obj.height, obj.size)
  }
}
