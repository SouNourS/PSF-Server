// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable.ListBuffer

class FireModeDefinition {
  var description : String = "a fire mode"
  var ammoTypes : ListBuffer[Ammo.Value] = new ListBuffer[Ammo.Value]
  var chamberSize : Int = 1
  var magazineSize : Int = 1
  var target : String = "other" //TODO: create applicable target types
}
