// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

class InventoryItem(val obj : Equipment, val x : Int, val y : Int) { }

object InventoryItem {
  def apply(obj : Equipment, x : Int, y : Int) = {
    new InventoryItem(obj, x, y)
  }

  def size : (Int, Int) {
    obj.getSize
  }
}
