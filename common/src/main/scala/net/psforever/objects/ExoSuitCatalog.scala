// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable

/**
  * A listing of all available exo-suit types by their GUID.
  * All references to the static values associated with the suit types should be made to their entries here.
  */
object ExoSuitCatalog {
  /**
    * A mapping of all of the hard defined exo-suits.
    */
  private val catalog : mutable.HashMap[Int, ExoSuit] = mutable.HashMap[Int, ExoSuit]()

  var armor : ExoSuit = _
  armor = ExoSuit(-1)
  armor.name = "no suit"
  armor.maxArmor = 0
  armor.inventoryWidth = 0
  armor.inventoryHeight = 0
  catalog += armor.guid -> armor

  armor = ExoSuit(0)
  armor.name = "standard exo-suit"
  armor.maxArmor = 50
  armor.inventoryWidth = 9
  armor.inventoryHeight = 6
  armor.holsterTypes(0) = EquipmentSize.PISTOL
  armor.holsterTypes(2) = EquipmentSize.RIFLE
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(1)
  armor.name = "agile exo-suit"
  armor.maxArmor = 100
  armor.inventoryWidth = 9
  armor.inventoryHeight = 9
  armor.holsterTypes(0) = EquipmentSize.PISTOL
  armor.holsterTypes(1) = EquipmentSize.PISTOL
  armor.holsterTypes(2) = EquipmentSize.RIFLE
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(2)
  armor.name = "reinforced exo-suit"
  armor.permission = 1 //TODO "reinforced exo-suit" certification needed
  armor.maxArmor = 200
  armor.inventoryWidth = 12
  armor.inventoryHeight = 9
  armor.holsterTypes(0) = EquipmentSize.PISTOL
  armor.holsterTypes(1) = EquipmentSize.PISTOL
  armor.holsterTypes(2) = EquipmentSize.RIFLE
  armor.holsterTypes(3) = EquipmentSize.RIFLE
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(3)
  armor.name = "infiltration suit"
  armor.permission = 1 //TODO "infiltration suit" certification needed
  armor.maxArmor = 0
  armor.inventoryWidth = 6
  armor.inventoryHeight = 6
  armor.holsterTypes(0) = EquipmentSize.PISTOL // TODO check that it is not pistol slot 1
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(4)
  armor.name = "mechanized assault exo-suit"
  armor.permission = 1 //TODO a "max" certification needed for suit, max weapons are permitted by other certification specifics
  armor.maxArmor = 650
  armor.inventoryWidth = 16
  armor.inventoryHeight = 16
  armor.holsterTypes(0) = EquipmentSize.MAX // TODO how to handle this?
  catalog += armor.guid -> armor

  /**
    * Reference an existing exo-suit from the catalog.
    * @param guid the globally unique identifier
    * @return the exo-suit
    */
  def get(guid : Int) : Option[ExoSuit] = {
    catalog.get(guid)
  }
}
