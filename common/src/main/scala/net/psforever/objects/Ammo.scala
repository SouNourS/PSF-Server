// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

object Ammo extends Enumeration {
  val
    FREE, //indicates that tool does not need to go through reload cycle before refire, e.g., knives
    BULLET_9MM,
    BULLET_9MM_AP,
    CELL_ENERGY
    = Value
}
