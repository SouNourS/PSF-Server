// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import net.psforever.packet.game.{PlanetSideEmpire, PlanetSideGUID}

import scala.collection.mutable.ListBuffer

/**
  * The representation of another person on the server.
  * Maintains both persistent properties, e.g., appearance, and temporary properties, e.g., life points, of the given player.
  * @param guid the globally unique id
  */
class PlayerAvatar(val guid : Int) extends PSGameObject {
  /** The name of the character, as seen by others. */
  var name : String = "player"
  /** The faction to which the character belongs. Defaults to tradition. */
  var faction : PlanetSideEmpire.Value = PlanetSideEmpire.TR
  /** The player's gender, which also affects their voice. Boolean, since there are only two options, and defaults to true (male). */
  var sex : Boolean = true
  /** The player's voice. Affected by sex. */
  var voice : Int = 0
  /** The player's style of face. Affected by sex. */
  var face : Int = 0

  /** The player's battle rank. Always a positive non-zero integer. */
  var br : Int = 1
  /** The amount of battle experience the player has earned since the last level.  Resets to zero upon earning a new rank. */
  var brExperience : Int = 0
  /** The player's command rank. Always, 0-5. */
  var cr : Int = 0
  /** The amount of command experience the player has earned since the last level.  Resets to zero upon earning a new rank. */
  var crExperience : Int = 0

  /** Life points. */
  var redHealth : Int = 100
  /** Energy for some actions, and implants. */
  var greenStamina : Int = 100
  /** Exo-suit-dependent resistance points for mitigating damage. */
  var blueArmor : Int = 0

  /** An index reference to the type of exo-suit the player is wearing. */
  private var suit : Int = -1
  /** Where equipment is stored for easy access on a player's person.  Permissions dependent on exo-suit type. */
  private val holsters : Array[EquipmentSlot] = Array.fill[EquipmentSlot](5)(new EquipmentSlot)
  /** Which holster is depicted as having its equipment drawn. Valid numbers are 0-4 and 255 (no holster drawn). */
  private var drawnHolster : Int = 255
  /** The player's personal inventory. */
  private val inventory : Backpack = Backpack(guid, 1,1)

  /** Whether the player is in a squatting position. */
  var crouched : Boolean = false

  /**
    * A constructor for player avatars that defines all of the character-creation properties.
    * Exo-suit type is defaulted to Standard Exo-suit for expedience, as that will trigger a setup process.
    * @constructor
    * @param guid the internal globally unique identifier for this player
    * @param aName the name of the character, as seen by others
    * @param aFaction the faction of the character
    * @param aSex the player's gender
    * @param aFace the player's voice
    * @param aVoice the player's style of face
    */
  def this(guid : Int, aName : String, aFaction : PlanetSideEmpire.Value, aSex : Boolean, aFace : Int, aVoice : Int) {
    this(guid)
    name = aName
    faction = aFaction
    sex = aSex
    face = aFace
    voice = aVoice
  }

  /**
    * Get the current health of the player.
    * @return the health
    */
  def getHealth : Int = {
    redHealth
  }

  /**
    * Set the player's health to this value.
    * Even when doing this, the new value is clamped between 0 and the maximum amount currently allowed.
    * @param newHealth the new value to replace current health
    */
  def setHealth(newHealth : Int) : Unit = {
    var points : Int = newHealth
    if(points < 0)
      points = 0
    else if(points > getMaxHealth)
      points = getMaxHealth
    redHealth = points
  }

  /**
    * Get the maximum health of the player that is permissible.
    * @return the maximum amount of health that a player may have at one time
    */
  def getMaxHealth : Int = {
    //TODO modified by biolab ownership
    //TODO modified by black ops
    //TODO modified by server population ratios?
    100
  }

  /**
    * Get the current stamina of the player.
    * @return the stamina
    */
  def getStamina : Int = {
    greenStamina
  }

  /**
    * Set the player's stamina to this value.
    * Even when doing this, the new value is clamped between 0 and the maximum amount currently allowed.
    * @param newStamina the new value to replace current stamina
    */
  def setStamina(newStamina : Int) : Unit = {
    var points : Int = newStamina
    if(points < 0)
      points = 0
    else if(points > getMaxStamina)
      points = getMaxStamina
    greenStamina = points
  }

  /**
    * Get the maximum stamina of the player that is permissible.
    * @return the maximum amount of atmina that a player may have at one time
    */
  def getMaxStamina : Int = {
    //TODO modified by black ops
    100
  }

  /**
    * Get the current armor rating of the player.
    * @return the armor value
    */
  def getPersonalArmor : Int = {
    blueArmor
  }

  /**
    * Set the player's armor rating to this value.
    * Even when doing this, the new value is clamped between 0 and the maximum amount currently allowed.
    * @param newArmor the new value to replace current armor rating
    */
  def setPersonalArmor(newArmor : Int) : Unit = {
    var points : Int = newArmor
    if(points < 0)
      points = 0
    else if(points > getMaxPersonalArmor)
      points = getMaxPersonalArmor
    blueArmor = points
  }

  /**
    * Get the maximum armor rating of the player that is permissible.
    * The maximum amount of armor is determined mainly by the player's exo-suit.
    * @return the maximum amount of armor that a player may have at one time
    */
  def getMaxPersonalArmor : Int = {
    //TODO modified by black ops
    ExoSuitCatalog.get(suit).get.maxArmor
  }

  /**
    * Get the exo-suit type by its catalog ID
    * @return the type of exo-suit the player is wearing
    */
  def getExoSuitType : Int = {
    suit
  }

  /**
    * Change the kind of exo-suit the player is wearing, or refresh the current exo-suit
    * @param esuit the catalog lookup of the exo-suit the player is changing into
    * @return a tuple containing (1) if the suit was changed and (2) a List of all the equipment that was dropped
    */
  def setExoSuitType(esuit : Int) : (Boolean, List[Equipment]) = {
    var swapped = suit != esuit
    var equipmentDropped: ListBuffer[Equipment] = ListBuffer()

    val aNewSuit: Option[ExoSuit] = ExoSuitCatalog.get(esuit)
    if(aNewSuit.isDefined) {
      suit = esuit
      val newSuit = aNewSuit.get
      setUsedHolster(255)
      if(swapped) {
        blueArmor = newSuit.maxArmor
        for(x <- 0 to 4) {
          val (_, equipment) = holsters(x).setSize(newSuit.holsterTypes(x))
          if(equipment.isDefined)
            equipmentDropped += equipment.get
        }
        //TODO resize and sort items in the inventory using a best-fit algorithm
        //TODO try to place some already dropped equipment back into a new, larger inventory
        //TODO remember to allocate dropped equipment from resized inventory
      }
    }
    else {
      // Not a valid exo-suit type
      swapped = false
    }
    (swapped, equipmentDropped.toList)
  }

  /**
    * What equipment slot does the player currently have out?
    * @return the number of the slot, or 255 for none
    */
  def getUsedHolster : Int = {
    drawnHolster
  }

  /**
    * Change whether a holster is drawn or put away.
    * Aside the values 0-4 to indicate which holster, the value 255 is used to indicate the "putting away" action
    * @param holster the number indicating which holster slot to draw, or 255
    * @throws IllegalArgumentException if the holster index does not exist
    * @return a tuple containing (1) if a holster was drawn or put away, and (2) what equipment is currently drawn
    */
  def setUsedHolster(holster : Int) : (Boolean, Option[Tool]) = {
    var swapped = false
    var equipmentOpt : Option[Tool] = None

    if(holster != drawnHolster) { // Swapping
      if(holster >= 0 && holster < 5) {
        if(drawnHolster != 255) {
          //TODO spontaneous weapon swap; do we flag this?
          //TODO if the packets have arrived out of order, what then?
        }
        if(holsters(holster).getEquipment.isDefined) {
          // If this holster is blocked by exo-suit type, there shouldn't be any equipment in it
          swapped = true
          drawnHolster = holster
          equipmentOpt = holsters(holster).getEquipment
        }
      }
      else if(holster == 255) {
        swapped = true
        drawnHolster = 255
      }
      else {
        //holster is either some weird number we don't handle or we already have it out
        throw new IllegalArgumentException("no holster - "+holster) //should we throw something much worse?
      }
    }
    else if(drawnHolster != 255) // What equipment do we currently have out
      equipmentOpt = holsters(drawnHolster).getEquipment
    (swapped, equipmentOpt)
  }

  /**
    * Get the equipment in a specifc holster.
    * @param holster the number indicating which holster slot to draw
    * @return the equipment in the holster, if any
    */
  def getEquipmentInHolster(holster : Int) : Option[Tool] = {
    val slot = getHolster(holster)
    val equipmentOpt : Option[Tool] = slot.getEquipment
    if(equipmentOpt.isEmpty && holster == drawnHolster) // Holster is drawn, but empty
      setUsedHolster(255) // Put away hand
    equipmentOpt
  }

  /**
    * Put the equipment into a specifc holster.
    * @param holster the number indicating which holster slot to draw
    * @param equipment a piece of Equipment that exists
    * @return the equipment in the holster, if any
    */
  def setEquipmentInHolster(holster : Int, equipment : Tool) : (Boolean, Option[Tool]) = {
    val slot = getHolster(holster)
    val (success, equip) = slot.setEquipment(equipment)
    if((success || slot.getEquipment.isEmpty) && holster == drawnHolster) // Holster is drawn, but empty
      setUsedHolster(255) // Put away hand
    (success, equip)
  }

  /**
    * Check a specific pocket on the player.
    * @param holster the holster slot number (0-4 are valid)
    * @throws IllegalArgumentException if the holster index does not exist
    * @return the equipment slot
    */
  def getHolster(holster : Int) : EquipmentSlot = {
    if(holster >= 0 && holster < 5)
      return holsters(holster)
    throw new IllegalArgumentException("no holster - "+holster) //should we throw something much worse?
  }

  /**
    * This method only complements the getter.
    * I'm not writing a proper Scaladoc for it.
    * __Never do this__.
    * ''I will find you''.
    */
  final def setHolster(holster : Int, newHolster : EquipmentSlot) : Unit = {
    throw new IllegalArgumentException(name+"'s limb replacement surgery disallowed")
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    PlayerAvatar.toString(this)
  }
}

object PlayerAvatar {
  /**
    * A constructor that accepts the minimum parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param guid the internal globally unique identifier for this player
    * @return the PlayerAvatar
    */
  def apply(guid : Int) : PlayerAvatar = {
    new PlayerAvatar(guid)
  }

  /**
    * An auxiliary constructor that accepts other parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param guid the internal globally unique identifier for this player
    * @param aName the name of the character, as seen by others
    * @param aFaction the faction of the character
    * @param aSex the player's gender
    * @param aFace the player's voice
    * @param aVoice the player's style of face
    * @return the PlayerAvatar
    */
  def apply(guid : Int, aName : String, aFaction : PlanetSideEmpire.Value, aSex : Boolean, aFace : Int, aVoice : Int) : PlayerAvatar = {
    new PlayerAvatar(guid, aName, aFaction, aSex, aFace, aVoice)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(player : PlayerAvatar) : String = {
    "{%s-%s-BR%d-CR%d}".format(player.name, player.faction.toString, player.br, player.cr)
  }
}
