// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import net.psforever.packet.game.PlanetSideGUID

class Backpack(val owner : PlanetSideGUID, width : Int, height : Int) extends ListInventory(width, height) {

  override def toString : String = {
    Backpack.toString(this)
  }
}

object Backpack {
  def apply(owner : PlanetSideGUID, x : Int, y : Int) : Backpack = {
    new Backpack(owner, x, y)
  }

  def toString(obj : Backpack) : String = {
    "[backpack: %dx%d, %d items]".format(obj.width, obj.height, obj.size)
  }
}
