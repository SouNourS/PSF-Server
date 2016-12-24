// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * A class the keeps track of canned width and height parameters used by the inventory representations of equipment.
  * @param width the width of the tile
  * @param height the height of the tile
  * @throws IllegalArgumentException if the width or height are zero or less
  */
class InventoryTile(val width : Int, val height : Int) {
  if(width <= 0 || height <= 0)
    throw new IllegalArgumentException("tile has no area - w=%d, h=%d".format(width, height))

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    InventoryTile.toString(this)
  }
}

object InventoryTile {
  /**
    * A constructor that accepts the minimum parameters.
    * @param height the height of the tile
    * @param width the width of the tile
    * @throws IllegalArgumentException if the width or height are zero or less
    */
  def apply(width : Int, height : Int) : InventoryTile = {
    new InventoryTile(width, height)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : InventoryTile) : String = {
    "{%dx%d}".format(obj.width, obj.height)
  }
}
