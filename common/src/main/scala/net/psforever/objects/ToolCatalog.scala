// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable

/**
  * A listing of all available tool types stored as ToolDefinitions alongside their GUID.
  * All references to the static values associated with the tool types should be made to their entries here.
  */
object ToolCatalog {
  private val catalog : mutable.HashMap[Int, ToolDefinition] = new mutable.HashMap[Int, ToolDefinition]

  tool = ToolDefinition(0, "beamer")
  tool.size = EquipmentSize.PISTOL
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.CELL_ENERGY
  tool.fireModes(0).magazineSize = 16
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.CELL_ENERGY
  tool.fireModes(1).magazineSize = 16
  tool.inventoryTileWidth = 3
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  var tool : ToolDefinition = _
  tool = ToolDefinition(1, "suppressor")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.BULLET_9MM
  tool.fireModes(0).ammoTypes += Ammo.BULLET_9MM_AP
  tool.fireModes(0).magazineSize = 25
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(2, "force blade")
  tool.size = EquipmentSize.MELEE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.FREE
  tool.fireModes(0).magazineSize = 1
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.FREE
  tool.fireModes(1).magazineSize = 1
  //should never end up in inventory; will match glitch knife dimensions, however, if it does
  catalog += tool.guid -> tool

  tool = ToolDefinition(3, "amp")
  tool.size = EquipmentSize.PISTOL
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.BULLET_9MM
  tool.fireModes(0).ammoTypes += Ammo.BULLET_9MM_AP
  tool.fireModes(0).magazineSize = 30
  tool.inventoryTileWidth = 3
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  /**
    * Reference an existing exo-suit from the catalog.
    * @param guid the globally unique identifier
    * @return the exo-suit
    */
  def get(guid : Int) : Option[ToolDefinition] = {
    Option(catalog(guid))
  }
}
