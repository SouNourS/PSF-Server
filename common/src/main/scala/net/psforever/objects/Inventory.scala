// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.util.control.Breaks._

import scala.collection.mutable.ListBuffer

class Inventory {
  private var width : Int = 0
  private var height : Int = 0
  private var contents : ListBuffer[InventoryItem] = new ListBuffer[InventoryItem]

  def this(w : Int, h : Int) {
    this()
    resize(w, h)
  }

  def addItem(item : Equipment, x : Int, y : Int) : (Boolean, Option[Equipment]) = {
    if(x < 0 || y < 0 || x + item.inventoryTileWidth > width || y + item.inventoryTileHeight > height)
      return (false, None)

    var overlap : List[Int] = testForOverlap(item, x, y)
    var success = true
    var swap : Option[Equipment] = None
    overlap.size match {
      case 0 =>
        contents += InventoryItem(item, x, y)
      case 1 =>
        swap = Option(contents.remove(overlap.head).obj)
        contents += InventoryItem(item, x, y)
      case _ =>
        success = false
    }
    (success, swap)
  }

  def testForOverlap(item : Equipment, x : Int, y : Int) : List[Int] = {
    var itemx0 = x
    var itemy0 = y
    var itemx1 = x + item.inventoryTileWidth
    var itemy1 = y + item.inventoryTileHeight

    var list : ListBuffer[Int] = new ListBuffer[Int]()
    for(i <- 0 to contents.size) {
      val stowed : InventoryItem = contents(i)
      val stowx0 = stowed.x
      val stowy0 = stowed.y
      val stowx1 = stowx0 + stowed.obj.inventoryTileWidth
      val stowy1 = stowy0 + stowed.obj.inventoryTileHeight
      if((itemx0 >= stowx0 && itemx0 <= stowx1) || (itemx1 >= stowx0 && itemx1 <= stowx1) &&
         (itemy0 >= stowy0 && itemy0 <= stowy1) || (itemy1 >= stowy0 && itemy1 <= stowy1)) {
          list += i
      }
    }
    list.toList
  }

  def removeItem(item : Equipment) : Option[Equipment] = {
    var removed : Option[Equipment] = None

    for(i <- 0 to contents.size) {
      val stowed : InventoryItem = contents(i)
      if(stowed.obj eq item) {
        removed = Option(contents.remove(i).obj)
        break
      }
    }
    removed
  }

  def removeItem(x : Int, y : Int) : Option[Equipment] = {
    var removed : Option[Equipment] = None

    for(i <- 0 to contents.size) {
      val stowed : InventoryItem = contents(i)
      val stowx0 = stowed.x
      val stowy0 = stowed.y
      val stowx1 = stowx0 + stowed.obj.inventoryTileWidth
      val stowy1 = stowy0 + stowed.obj.inventoryTileHeight
      if((x >= stowx0 && x <= stowx1) || (y >= stowy0 && y <= stowy1)) {
        removed = Option(contents.remove(i).obj)
        break
      }
    }
    removed
  }

  private def resize(w : Int, h : Int) : List[Equipment] = {
    width = w
    height = h

    if(contents.size > 0) {
      var temp : List[InventoryItem] = contents.toList
      var dropped : ListBuffer[Equipment] = new ListBuffer[Equipment]
      contents.clear
      //TODO space-fitting algorithm to push equipment back into inventory within its new constraints
      //TODO elements that don't fit will get dropped
      return dropped.toList
    }
    return Nil
  }
}

object Inventory {
  def apply(w : Int, h : Int) = {
    new Inventory(w, h)
  }
}
