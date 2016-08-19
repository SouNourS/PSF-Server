// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable.ListBuffer

class FireModeDefinition {
  var ammoTypes : ListBuffer[Ammo] = ListBuffer[Ammo]
  var chamber : Int = 1
  var magazineSize : Int = 1
}
