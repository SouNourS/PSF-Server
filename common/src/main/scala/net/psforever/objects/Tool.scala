// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class Tool(override val guid : Int, var toolDef : Int) extends Equipment(guid) {
  getToolDefinition //test for toolDef validity

  var fireModeIndex : Int = 0
  var ammoTypeIndex : Int = 0
  var magazine : Int = 0
  var isHeld : Boolean = false

  def this(guid : Int, toolDef : Int, x : Float, y : Float, z : Float) = {
    this(guid, toolDef)
    setPosition(x, y, z)
  }

  override def getName : String = {
    getToolDefinition.name
  }

  override def getSize : EquipmentSize.Value = {
    getToolDefinition.size
  }

  def getToolDefinition : ToolDefinition = {
    val defin : Option[ToolDefinition] = ToolCatalog.get(toolDef)
    if(defin.isEmpty) {
      throw new IllegalArgumentException("invalid tool definition - "+toolDef)
    }
    defin.get
  }

  def setToolDefinition(defin : Int) : Boolean = {
    if(ToolCatalog.get(defin).isEmpty)
      return false
    toolDef = defin
    true
  }

  def getFireMode : FireModeDefinition = {
    getToolDefinition.fireModes(fireModeIndex)
  }

  def getFireModeIndex : Int = {
    fireModeIndex
  }

  def setFireModeIndex(mode : Int) : Boolean = {
    val defin : ToolDefinition = getToolDefinition
    if(mode != fireModeIndex) {
      val modes : ListBuffer[FireModeDefinition] = defin.fireModes
      if(mode < 0 || mode > modes.size)
        return false

      // reset fire mode
      val curMode : FireModeDefinition = defin.fireModes(fireModeIndex)
      fireModeIndex = mode
      val newMode : FireModeDefinition = defin.fireModes(mode)

      // reset ammunition
      if(curMode.ammoTypes(ammoTypeIndex) != newMode.ammoTypes(ammoTypeIndex)) {
        val usedAmmo : Ammo.Value = curMode.ammoTypes(ammoTypeIndex)
        val newAmmoTypes : ListBuffer[Ammo.Value] = newMode.ammoTypes
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
      val fireMode : FireModeDefinition = getToolDefinition.fireModes(fireModeIndex)
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
    val tool : ToolDefinition = getToolDefinition
    (tool.inventoryTileWidth, tool.inventoryTileHeight)
  }

  override def toString : String = {
    Tool.toString(this)
  }
}

object Tool {
  def apply(guid : Int, toolDef : Int) = {
    new Tool(guid, toolDef)
  }

  def apply(guid : Int, toolDef : Int, x : Float, y : Float, z : Float) = {
    new Tool(guid, toolDef, x, y, z)
  }

  def toString(obj : Tool) : String = {
    "{%s-%s(%d/%d)-fire:%d}".format(obj.getName, obj.getAmmoType.toString, obj.magazine, obj.getFireMode.magazineSize, obj.getFireModeIndex)
  }
}
