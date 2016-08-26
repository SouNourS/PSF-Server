// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

trait Inventory {
  def size : Int
  def addItem(item : Equipment, x : Int, y : Int) : (Boolean, Option[Equipment])
  def removeItem(item : Equipment) : Option[Equipment]
  def removeItem(x : Int, y : Int) : Option[Equipment]
  def resize(w : Int, h : Int) : List[Equipment]
}
