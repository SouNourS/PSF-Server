// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * A player character's backpack space.
  * @param owner the player character's GUID
  * @param width the width
  * @param height the height
  */
class Backpack(val owner : Int, width : Int, height : Int) extends ListInventory(width, height) {
  /**
    * The indexOffset of the backpack is 134 (decimal) or 86 (hex).
    * The first cell is either (0,0) or 134 / 0x86.
    * This is necessary for calls to Inventory.fromHexIndex.
    */
  override val indexOffset = 134

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    Backpack.toString(this)
  }
}

object Backpack {
  /**
    * A constructor that accepts the minimum parameters.
    * @param owner the player character's GUID
    * @param width the width
    * @param height the height
    * @return the Backpack
    */
  def apply(owner : Int, width : Int, height : Int) : Backpack = {
    new Backpack(owner, width, height)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Backpack) : String = {
    "{backpack(%dx%d): %d items}".format(obj.width, obj.height, obj.size)
  }
}
