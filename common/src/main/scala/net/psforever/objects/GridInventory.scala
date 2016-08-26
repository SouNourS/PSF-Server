// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.annotation.switch
import scala.util.control.Breaks._
import scala.collection.mutable

class GridInventory(var width : Int, var height : Int) extends Inventory {
  protected var contents : mutable.HashMap[Int,InventoryItem] = new mutable.HashMap[Int,InventoryItem]
  protected var grid : Array[Int] = _
  resize(width, height) // Call to setup storage space, as need be

  def size : Int = {
    contents.size
  }

  def addItem(item : Equipment, x : Int, y : Int) : (Boolean, Option[Equipment]) = {
    if(x < 0 || y < 0 || x + item.getInventorySize._1 > width || y + item.getInventorySize._2 > height)
      return (false, None)

    val overlap : List[Int] = testForOverlap(item, x, y)
    var success = true
    var swap : Option[Equipment] = None
    (overlap.size: @switch) match {
      case 0 =>
        contents += (item.guid -> InventoryItem(item, x, y))
        val nsize = item.getInventorySize
        setRange(x, y, nsize._1, nsize._2, item.guid)

      case 1 =>
        val swapopt = contents.remove(overlap.head).get
        swap = Option(swapopt.obj)
        if(swap.isDefined) {
          val isize = swap.get.getInventorySize
          setRange(swapopt.x, swapopt.y, isize._1, isize._2)
        }
        contents += (item.guid -> InventoryItem(item, x, y))
        val nsize = item.getInventorySize
        setRange(x, y, nsize._1, nsize._2, item.guid)

      case _ =>
        success = false
    }
    (success, swap)
  }

  def testForOverlap(item : Equipment, x : Int, y : Int) : List[Int] = {
    val xw = x + item.getInventorySize._1
    val yh = y + item.getInventorySize._2
    var list: mutable.HashMap[Int,Boolean] = new mutable.HashMap[Int,Boolean]

    for(gy <- x to yh) {
      for(gx <- y to xw) {
        val index : Int = grid(gy * width + gx)
        if(grid(index) > -1)
          list += (index -> true)
      }
    }
    list.keySet.toList
  }

  private def setRange(x : Int, y : Int, w : Int, h : Int, value : Int = -1) : Boolean = {
    val xw : Int = x + w
    val yh : Int = y + h
    if(w < 0 || h < 0 || w < x || h < y || xw > width || yh > height)
      return false

    for(gy <- x to xw) {
      for(gx <- y to yh) {
        val index : Int = grid(gy * width + gx)
        grid(index) = value
      }
    }
    true
  }

  def removeItem(item : Equipment) : Option[Equipment] = {
    contents.foreach { case (_: Int, tile: InventoryItem) =>
      if(tile.obj eq item) {
        return removeItem(tile.x, tile.y)
      }
    }
    None
  }

  def removeItem(x : Int, y : Int) : Option[Equipment] = {
    var removed: Option[Equipment] = None

    val guid : Int = grid(y * width + x)
    if(guid > -1 ) {
      val invopt = contents.get(guid)
      var rx : Int = x
      var ry : Int = y
      var rw : Int = 1
      var rh : Int = 1
      if(invopt.isDefined) {
        val invitem = invopt.get
        removed = Option(invitem.obj)
        rx = invitem.x
        ry = invitem.y
        rw = invitem.obj.getInventorySize._1
        rh = invitem.obj.getInventorySize._2
      }
      setRange(rx, ry, rw, rh)
    }
    removed
  }

  def resize(w : Int, h : Int) : List[Equipment] = {
    val oldWidth : Int = width
    val oldHeight : Int = height
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
