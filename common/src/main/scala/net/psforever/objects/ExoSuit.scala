// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * A constructed example of the personal armor the player wears.
  * Players are influenced by the exo-suit they wear in a variety of ways, with speed and available equipment slots being major differences.
  * No data should (need to) be modified after a suit object is created and initialized with values.
  * @param guid the globally unique id
  */
class ExoSuit(val guid : Int) {
  /** Cosmetic name of the exo-suit */
  var name : String = "exo-suit"
  /** Any certification requirements to obtain this suit. */
  var permission : Int = 0 //TODO certification type?
  /** The maximum amount of personal armor granted by the suit. */
  var maxArmor : Int = 0
  /** The horizontal span of the suit's allotted inventory, in grid boxes. */
  var inventoryWidth = 0
  /** The vertical span of the suit's allotted inventory, in grid boxes. */
  var inventoryHeight = 0
  /**
    * Internally, the holsters are numerically assigned to the values:<br>
    * 0 -> Pistol slot 1<br>
    * 1 -> Pistol slot 2<br>
    * 2 -> Rifle slot 1<br>
    * 3 -> Rifle slot 2<br>
    * 4 -> Melee slot<br>
    * The value 255 is reserved to indicate putting drawn equipment away.
    * This is true for all infantry exo-suits, even if that slot doesn't have a slot available (BLOCKED).
    */
  var holsterTypes : Array[EquipmentSize.Value] = Array.fill[EquipmentSize.Value](5)(EquipmentSize.BLOCKED)

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    ExoSuit.toString(this)
  }
}

object ExoSuit {
  /**
    * A constructor that accepts the minimum parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param guid the globally unique id
    * @return the ExoSuit
    */
  def apply(guid : Int) = {
    new ExoSuit(guid)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(suit : ExoSuit) : String = {
    "{%s}".format(suit.name)
  }
}
