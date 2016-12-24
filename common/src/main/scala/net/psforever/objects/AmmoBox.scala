// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

class AmmoBox(guid : Int, var ammo : Ammo.Value, var quantity : Int) extends Equipment(guid) {
  def getAmmoType : Ammo.Value = {
    ammo
  }

  def setAmmoType(ammot : Ammo.Value) : Unit = {
    ammo = ammot
  }

  def getQuantity : Int = {
    quantity
  }

  def setQuantity(quant : Int) : Unit = {
    quantity = math.min(quant, 0)
  }

  /**
    * Remove a certain amount of ammunition from this container.
    * @param rounds the number of projectiles needed
    * @return the number of projectiles acquired from this box
    */
  def reloadRequest(rounds : Int) : Int = {
    var ret : Int = 0
    if(rounds >= quantity) {
      ret = quantity
      quantity = 0
    }
    else {
      ret = rounds
      quantity -= rounds
    }
    ret
  }

  /**
    * Return the dimensions of the inventory representation for this piece of equipment.
    */
  override def getInventorySize : (Int, Int) = {
    (3, 3)
  }

  override def toString : String = {
    AmmoBox.toString(this)
  }
}

object AmmoBox {
  def apply(guid :Int, ammo : Ammo.Value, quantity : Int) : AmmoBox = {
    new AmmoBox(guid, ammo, quantity)
  }

  def toString(obj : AmmoBox) : String = {
    "{%s-box(%d)}".format(obj.ammo.toString, obj.quantity)
  }
}
