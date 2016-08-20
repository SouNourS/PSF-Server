// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.annotation.switch
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

class Inventory(var width : Int, var height : Int) {
  private var contents : ListBuffer[InventoryItem] = new ListBuffer[InventoryItem]
  resize(width, height) // Call to setup storage space, as need be

  def addItem(item : Equipment, x : Int, y : Int) : (Boolean, Option[Equipment]) = {
    if(x < 0 || y < 0 || x + item.getInventorySize._1 > width || y + item.getInventorySize._2 > height)
      return (false, None)

    val overlap : List[Int] = testForOverlap(item, x, y)
    var success = true
    var swap : Option[Equipment] = None
    (overlap: @switch).size match {
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
    val itemx0 = x
    val itemy0 = y
    val itemx1 = x + item.getInventorySize._1
    val itemy1 = y + item.getInventorySize._2

    var list : ListBuffer[Int] = new ListBuffer[Int]()
    for(i <- 0 to contents.size) {
      val stowed : InventoryItem = contents(i)
      val stowx0 = stowed.x
      val stowy0 = stowed.y
      val stowx1 = stowx0 + stowed.obj.getInventorySize._1
      val stowy1 = stowy0 + stowed.obj.getInventorySize._2
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
      val stowx1 = stowx0 + stowed.obj.getInventorySize._1
      val stowy1 = stowy0 + stowed.obj.getInventorySize._2
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

    if(contents.nonEmpty) {
      var temp : List[InventoryItem] = contents.toList
      var dropped : ListBuffer[Equipment] = new ListBuffer[Equipment]
      contents.clear
      //TODO space-fitting algorithm to push equipment back into inventory within its new constraints
      //TODO elements that don't fit will get dropped
      return dropped.toList
    }
    Nil
  }

  override def toString : String = {
    Inventory.toString(this)
  }
}

object Inventory {
  def apply(w : Int, h : Int) : Inventory = {
    new Inventory(w, h)
  }

  def toString(obj : Inventory) : String = {
    "[inventory: %dx%d, %d items]".format(obj.width, obj.height, obj.contents.size)
  }
}
