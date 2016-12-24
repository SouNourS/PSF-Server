// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * A checked unit of storage for equipment.
  * If the equipment is not the correct size, it will not be stored.
  * If the size changes, any stored equipment that is the wrong size will be dropped.
  */
class EquipmentSlot {
  /** The inclusive "capacity" of this slot. */
  private var size : EquipmentSize.Value = EquipmentSize.BLOCKED
  /** What kind of equipment is stored in this slot. */
  private var equip : Tool = _

  /**
    * A constructor for equipment slots that can start with an initial functional capacity for the slot.
    * @constructor
    * @param esize the capacity
    */
  def this(esize : EquipmentSize.Value) {
    this()
    size = esize
  }

  /**
    * Get the capacity for this slot
    * @return the capacity
    */
  def getSize : EquipmentSize.Value = {
    size
  }

  /**
    * Change the size of the equipment that can be placed into this slot.
    * If equipment currently in the slot no longer fits, we must remove it.
    * @param esize the new size of the equipment that can be placed in this slot
    * @return a tuple containg (1) if the size was changed and (2) what tool was removed from the slot
    */
  def setSize(esize : EquipmentSize.Value) : (Boolean, Option[Tool]) = {
    var tool : Option[Tool] = None
    if(Option(equip).isDefined && (size != esize || esize == EquipmentSize.BLOCKED)) {
      tool = Option(equip)
      equip = null
    }
    size = esize
    (true, tool)
  }

  /**
    * Get any equipment in this slot.
    * @return an Option that contains the equipment
    */
  def getEquipment : Option[Tool] = {
    Option(equip)
  }

  /**
    * Swap the equipment in this slot.
    * This method handles both insertion and removal.
    * @param equipment a piece of Equipment that exists
    * @return a tuple containg (1) if the equipment was put into the slot and (2) what equipment was removed in the slot, if any
    */
  def setEquipment(equipment : Tool) : (Boolean, Option[Tool]) = {
    if(size != EquipmentSize.BLOCKED) {
      val held : Option[Tool] = Option(equip)
      val equipmentOpt : Option[Tool] = Option(equipment)
      if((equipmentOpt.isDefined && equipment.getSize != EquipmentSize.BLOCKED && equipment.getSize == size) || (equipmentOpt.isEmpty && held.isDefined)) {
        if(equipmentOpt.isDefined) {
          equip = equipment
        }
        else
          equip = null
        return (true, held)
      }
    }
    (false, None)
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    EquipmentSlot.toString(this)
  }
}

object EquipmentSlot {
  /**
    * A constructor that accepts the minimum parameters and does not need to be invoked with a literal "new."
    * @constructor
    * @param esize the capacity
    * @return the EquipmentSlot
    */
  def apply(esize : EquipmentSize.Value) = {
    new EquipmentSlot(esize)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : EquipmentSlot) : String = {
    var equipStr = ""
    if(obj.getEquipment.isDefined)
      equipStr = "-equipment:%s".format(obj.getEquipment.get.toString)
    "{EquipmentSlot-type:%s%s}".format(obj.getSize.toString, equipStr)
  }
}
