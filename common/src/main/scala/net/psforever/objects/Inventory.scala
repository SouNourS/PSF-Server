// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.util.{Failure, Success, Try}

abstract class Inventory(var width : Int, var height : Int) {
  val topLeftCorner : Int = 134

  def size : Int
  def capacity : Int = width * height

  def addItem(item : Equipment, hex : Int) : (Boolean, Option[Equipment]) = {
    val hexConversion = Inventory.fromHexIndex(this, hex)
    hexConversion match {
      case Success(coords) =>
        return addItem(item, coords._1, coords._2)
      case Failure(err) =>
        err.printStackTrace()
    }
    (false, None)
  }

  def addItem(item : Equipment, y : Int, x : Int) : (Boolean, Option[Equipment])
  def getItem(guid : Int) : Option[Equipment]
  def getItem(item : Equipment) : Option[Equipment]
  def getItem(y : Int, x : Int) : Option[Equipment]
  def removeItem(guid : Int): Option[Equipment]
  def removeItem(item : Equipment): Option[Equipment]
  def removeItem(y : Int, x : Int): Option[Equipment]
  def resize(w : Int, h : Int): List[Equipment]
}

object Inventory {
  def toHexIndex(obj : Inventory, y : Int, x : Int) : Try[Int] = {
    if(y < 0 || y >= obj.height || x < 0 || x >= obj.width)
      throw new IllegalArgumentException("coordinates off grid - y:0 <= %d < %d, or x:0 <= %d < %d".format(y, obj.height, x, obj.width))
    Try(obj.topLeftCorner + y * obj.width + x)
  }

  /**
    * na
    * Thank you for the toHexString method, Java.
    * @param obj
    * @param hex
    * @return
    */
  def fromHexIndex(obj : Inventory, hex : Int) : Try[(Int, Int)] = {
    val objTopLeftCorner = obj.topLeftCorner
    val value = hex - objTopLeftCorner
    if(value < 0)
      throw new IllegalArgumentException("coordinate too early for inventory - %s < %s".format(hex.toHexString, objTopLeftCorner.toHexString))
    else if(value == 0)
      return Try((0, 0))

    val y : Int = math.floor(value / obj.width).toInt
    if(y >= obj.height)
      throw new IllegalArgumentException("coordinate too late for inventory - %s > %s".format(hex.toHexString, (objTopLeftCorner + obj.width * obj.height - 1).toHexString))
    val x : Int = value - (y * obj.width)
    Try((y, x))
  }
}
