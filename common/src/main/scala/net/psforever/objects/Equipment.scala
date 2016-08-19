// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * The basic representation of equipment that can be used by the player.
  * @param guid the globally unique id
  * @param size the exclusive "load"
  */
class Equipment(val guid : Int, val size : EquipmentSize.Value = EquipmentSize.BLOCKED) extends PSGameObject {
  /** Temporary cosmetic field. */
  var name : String = "equipment"
  var inventoryTileWidth : Int = 1
  var inventoryTileHeight : Int = 1

  /**
    * A constructor for equipment that indicates where the entity is spawned in the world.
    * @constructor
    * @param guid the globally unique id
    * @param size the exclusive "load"
    * @param x the x-coordinate of the location
    * @param y the y-coordinate of the location
    * @param z the z-coordinate of the location
    */
  def this(guid : Int, size : EquipmentSize.Value, x : Float, y : Float, z : Float) = {
    this(guid, size)
    setPosition(x, y, z)
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
    * A constructor that accepts the minimum parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param guid the globally unique id
    * @param size the exclusive "load"
    * @return the Equipment
    */
  def apply(guid : Int, size : EquipmentSize.Value) = {
    new Equipment(guid, size)
  }

  /**
    * A constructor that accepts the minimum parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param guid the globally unique id
    * @param size the exclusive "load"
    * @param x the x-coordinate of the location
    * @param y the y-coordinate of the location
    * @param z the z-coordinate of the location
    * @return the Equipment
    */
  def apply(guid : Int, size : EquipmentSize.Value, x : Float, y : Float, z : Float) = {
    new Equipment(guid, size, x, y, z)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Equipment) : String = {
    "{%s-pos:%s}".format(obj.name, PSGameObject.toString(obj))
  }
}
