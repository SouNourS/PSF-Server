// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

class InventoryItem(val obj : Equipment, val y : Int, val x : Int) {
  if(Option(obj).isEmpty)
    throw new IllegalArgumentException("can not build an inventory container without an item to be contained")

  def getInventorySize : (Int, Int) = {
    obj.getInventorySize
  }

  override def toString : String = {
    InventoryItem.toString(this)
  }
}

object InventoryItem {
  def apply(obj : Equipment, y : Int, x : Int) = {
    new InventoryItem(obj, y, x)
  }

  def toString(obj : InventoryItem) : String = {
    "<%s>".format(obj.obj.toString)
  }
}
