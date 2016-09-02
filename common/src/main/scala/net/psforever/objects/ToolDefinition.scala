// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable.ListBuffer

class ToolDefinition(val guid : Int, val name : String) {
  var fireModes : ListBuffer[FireModeDefinition] = new ListBuffer[FireModeDefinition]
  var size : EquipmentSize.Value = EquipmentSize.BLOCKED
  var inventoryTileWidth : Int = 1
  var inventoryTileHeight : Int = 1
}

object ToolDefinition {
  def apply(guid: Int, name :String) = {
    new ToolDefinition(guid, name)
  }
}