// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class Tool(override val guid : Int, override val size : EquipmentSize.Value, val toolDef : Int) extends Equipment(guid, size) {
  if(ToolCatalog.get(toolDef).isEmpty)
    throw new IllegalArgumentException("invalid tool definition - "+toolDef)

  var fireModeIndex : Int = 0
  var ammoTypeIndex : Int = 0
  var magazine : Int = 0
  var isHeld : Boolean = false

  def this(guid : Int, size : EquipmentSize.Value, toolDef : Int, x : Float, y : Float, z : Float) = {
    this(guid, size, toolDef)
    setPosition(x, y, z)
  }

  def getFireMode : FireModeDefinition = {
    ToolCatalog.get(toolDef).get.fireModes(fireModeIndex)
  }

  def getFireModeIndex : Int = {
    fireModeIndex
  }

  def setFireModeIndex(mode : Int) : Boolean = {
    if(mode != fireModeIndex) {
      val modes : ListBuffer[FireModeDefinition] = ToolCatalog.get(toolDef).get.fireModes
      if(mode < 0 || mode > modes.size)
        return false

      // reset fire mode
      val curMode : FireModeDefinition = ToolCatalog.get(toolDef).get.fireModes(fireModeIndex)
      fireModeIndex = mode
      val newMode : FireModeDefinition = ToolCatalog.get(toolDef).get.fireModes(mode)

      // reset ammunition
      if(curMode.ammoTypes(ammoTypeIndex) != newMode.ammoTypes(ammoTypeIndex)) {
        var usedAmmo : Ammo.Value = curMode.ammoTypes(ammoTypeIndex)
        var newAmmoTypes : ListBuffer[Ammo.Value] = newMode.ammoTypes
        ammoTypeIndex = 0 // set default
        for(x <- 0 to newAmmoTypes.size) {
          if(newAmmoTypes(x) == usedAmmo) {
            ammoTypeIndex = x
            break
          }
        }
      }
      return true
    }
    false
  }

  def getAmmoType : Ammo.Value = {
    getFireMode.ammoTypes(ammoTypeIndex)
  }

  def getAmmoTypeIndex : Int = {
    ammoTypeIndex
  }

  def setAmmoTypeIndex(ammo : Int) : Boolean = {
    if(ammo != fireModeIndex) {
      val fireMode : FireModeDefinition = ToolCatalog.get(toolDef).get.fireModes(fireModeIndex)
      if(ammo >= 0 || ammo < fireMode.ammoTypes.size) {
        ammoTypeIndex = ammo
        return true
      }
    }
    false
  }

  /**
    * Return the dimensions of the inventory representation for this piece of equipment.
    */
  override def getInventorySize : (Int, Int) = {
    val tool : ToolDefinition = ToolCatalog.get(toolDef).get
    (tool.inventoryTileWidth, tool.inventoryTileHeight)
  }

  override def toString : String = {
    Tool.toString(this)
  }
}

object Tool {
  def apply(guid : Int, size : EquipmentSize.Value, toolDef : Int) = {
    new Tool(guid, size, toolDef)
  }

  def apply(guid : Int, size : EquipmentSize.Value, toolDef : Int, x : Float, y : Float, z : Float) = {
    new Tool(guid, size, toolDef, x, y, z)
  }

  def toString(obj : Tool) : String = {
    "Tool"
  }
}
