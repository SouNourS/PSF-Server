// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.util.control.Breaks._

class Tool(val guid : Int, val size : EquipmentSize.Value, val objDef : ToolDefintion) extends Equipment(guid, size) {
  var fireModeIndex : Int = 0
  var ammoTypeIndex : Int = 0
  var magazine : Int = 0
  var isHeld : Boolean = false

  def this(guid : Int, size : EquipmentSize.Value, objDef : ToolDefintion, x : Float, y : Float, z : Float) = {
    this(guid, size, objDef)
    setPosition(x, y, z)
  }

  def getFireMode : FireModeDefinition = {
    objDef.fireModes(fireModeIndex)
  }

  def getFireModeIndex : Int = {
    fireModeIndex
  }

  def setFireModeIndex(mode : Int) : Boolean = {
    if(mode != fireModeIndex) {
      val modes : List[FireModeDefinition] = objDef.fireModes
      if(mode < 0 || mode > modes.size)
        return false

      // reset fire mode
      val curMode : ToolDefinition = objDef.fireModes(fireModeIndex)
      fireModeIndex = mode
      val newMode : ToolDefinition = objDef.fireModes(mode)

      // reset ammunition
      if(curMode.ammoTypes(ammoTypeIndex) != newMode.ammoTypes(ammoTypeIndex)) {
        var usedAmmo : Ammo = curMode.ammoTypes(ammoTypeIndex)
        var newAmmoTypes : List[Ammo] = newMode.ammoTypes
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

  def getAmmoType : Ammo = {
    getFireMode.ammoTypes(ammoTypeIndex)
  }

  def getAmmoTypeIndex : Int = {
    ammoTypeIndex
  }

  def setAmmoTypeIndex(ammo : Int) : Boolean = {
    if(ammo != fireModeIndex) {
      val fireMode : FireModeDefinition = objDef.fireModes(fireModeIndex)
      if(ammo >= 0 || ammo < fireMode.ammoTypes.size) {
        ammoTypeIndex = ammo
        return true
      }
    }
    false
  }

  override def toString : String = {
    Tool.toString(this)
  }
}

object Tool {
  def apply(guid : Int, size : EquipmentSize.Value, objDef : ToolDefintion) = {
    new Tool(guid, size, objDef)
  }

  def apply(guid : Int, size : EquipmentSize.Value, objDef : ToolDefintion, x : Float, y : Float, z : Float) = {
    new Tool(guid, size, objDef, x, y, z)
  }

  def toString(obj : Tool) : String = {
    "Tool"
  }
}
