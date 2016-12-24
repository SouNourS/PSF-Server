// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * A list of available the equipment sizes.
  * This represents a strictly fitted system - smaller things are not intended to fit into larger containers.
  */
object EquipmentSize extends Enumeration {
  val BLOCKED, //unusable
      MELEE, //special
      PISTOL, //2x2, 2x3, 3x3
      RIFLE, //6x3 and 9x3
      MAX, //max weapon only
      ANY //currently unused
  = Value
}
