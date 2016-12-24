// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable

/**
  * A listing of all available inventory tile dimensions by their descriptive strings.
  * All Equipment should have a stock inventory tile size listed and referenced herein.
  */
object InventoryTileCatalog {
  /**
    * A mapping of all of the hard defined tile dimensions.
    */
  private val catalog : mutable.HashMap[String, InventoryTile] = mutable.HashMap[String, InventoryTile]()

  catalog += "1x1" -> InventoryTile(1,1) // e.g., glitched knives
  catalog += "2x2" -> InventoryTile(2,2) // e.g., grenades
  catalog += "2x3" -> InventoryTile(2,3) // e.g., medical canister
  catalog += "3x2" -> InventoryTile(3,2) // e.g., medkits
  catalog += "3x3" -> InventoryTile(3,3) // e.g., pistols
  catalog += "4x4" -> InventoryTile(4,4) // e.g., large ammunition boxes
  catalog += "6x3" -> InventoryTile(6,3) // e.g., medium assault rifles
  catalog += "9x3" -> InventoryTile(9,3) // e.g., heavy assault weapons

  catalog.withDefaultValue(catalog("1x1")) // Add as a default

  /**
    * Reference existing dimensions of an inventory tile from the catalog.
    * Technically, since there is a default value, a tile will always be retrieved from the catalog.
    * @param dimensions a String that represents the tile dimensions in the form width x height
    * @return the tile
    */
  def get(dimensions : String) : Option[InventoryTile] = {
    catalog.get(dimensions)
  }
}

