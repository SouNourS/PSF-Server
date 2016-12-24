// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * The basic representation of equipment that can be used by the player.
  * @param guid the globally unique id
  */
class Equipment(val guid : Int) extends PSGameObject {
  /**
    * A constructor for equipment that indicates where the entity is spawned in the world.
    * @constructor
    * @param guid the globally unique id
    * @param x the x-coordinate of the location
    * @param y the y-coordinate of the location
    * @param z the z-coordinate of the location
    */
  def this(guid : Int, x : Float, y : Float, z : Float) = {
    this(guid)
    setPosition(x, y, z)
  }

  /**
    * Get the common name of the equipment.
    * @return the name of the equipment
    */
  def getName : String = {
    "Equipment"
  }

  /**
    * Set a common name for the equipment.
    * Override this method when useful.
    * @param ename the new name
    */
  def setName(ename : String) : Unit = { }

  /**
    * Get the required holster size for this peice of equipment.
    * @return the size
    */
  def getSize : EquipmentSize.Value = {
    EquipmentSize.BLOCKED
  }

  /**
    * Set the required holster size for this peice of equipment.
    * Override this method when useful.
    * @param esize the new size
    */
  def setSize(esize : EquipmentSize.Value) : Unit = { }

  /**
    * Return the dimensions of the inventory representation for this piece of equipment.
    * @return a Tuple containing (1) the width of the tile and (2) the height of the tile
    */
  def getInventorySize : (Int, Int) = {
    (1, 1)
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    Equipment.toString(this)
  }
}

object Equipment {
  /**
    * A constructor that accepts the minimum parameters.
    * @constructor
    * @param guid the globally unique id
    * @return the Equipment
    */
  def apply(guid : Int) = {
    new Equipment(guid)
  }/**
    * A constructor that accepts the minimum parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param guid the globally unique id
    * @param x the x-coordinate of the location
    * @param y the y-coordinate of the location
    * @param z the z-coordinate of the location
    * @return the Equipment
    */
  def apply(guid : Int, x : Float, y : Float, z : Float) = {
    new Equipment(guid, x, y, z)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Equipment) : String = {
    "{%s}".format(obj.getName)
  }
}
