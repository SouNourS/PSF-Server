// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import net.psforever.types.Vector3

/**
  * A basic subclass that indicates an entity that exists somewhere in the world.
  */
class PSGameObject {
  /** x-coordinate; typically between 0-4000 and is a WE distinction */
  var posX : Float = 0f
  /** y-coordinate; typically between [Range] and is an altitude distinction */
  var posY : Float = 0f
  /** z-coordinate; typically between 0-4000 and is a NS distinction */
  var posZ : Float = 0f
  /** Angle of rotation about a vector perpendicular to the ground (ZX-plane). */
  var aimYaw : Float = 0f //TODO should we store this value in radians?
  /** Angle of rotation about a vector coplanar with the ground (ZX-plane). */
  var aimPitch : Float = 0f //TODO should we store this value in radians?

  /**
    * A constructor that defines where the entity is.
    * @param x the x-coordinate
    * @param y the y-coordinate
    * @param z the z-coordinate
    */
  def this(x : Float, y : Float, z : Float) {
    this()
    posX = x
    posY = y
    posZ = z
  }

  def this(x : Float, y: Float, z : Float, yaw : Float, pitch : Float) {
    this(x, y, z)
    setYaw(yaw)
    setPitch(pitch)
  }

  /**
    * Make the coordinates easily accessible in one place.
    * @return a Vector3 containing the current position
    */
  def getPosition : Vector3 = {
    Vector3(posX, posY, posZ)
  }

  /**
    * Set the position to the coordinate values.
    * @param x the new x-coordinate, defaulted to an unchanged value
    * @param y the new y-coordinate, defaulted to an unchanged value
    * @param z the new z-coordinate, defaulted to an unchanged value
    */
  def setPosition(x : Float = posX, y : Float = posY, z : Float = posZ) : Unit = {
    posX = x
    posY = y
    posZ = z
  }

  /**
    * Set the position to the coordinate values.
    * @param vec a vector3 containing the new coordinates
    */
  def setPosition(vec : Vector3) : Unit = {
    posX = vec.x
    posY = vec.y
    posZ = vec.z
  }

  /**
    * Get the orientation of the object respective to the perpendicular of the ground (ZX-plane).
    * @return a positive value typically between >=0 degrees and <360 degrees.
    */
  def getYaw : Float = {
    aimYaw
  }

  /**
    * Set the orientation of the object respective to the perpendicular of the ground (ZX-plane).
    * @param look a positive value clamped between n >= 0 degrees and n < 360 degrees
    */
  def setYaw(look : Float) : Unit = {
    var dLook : Float = look
    dLook %= 360f
    if(look < 0)
      dLook += 360
    aimYaw = dLook
  }

  /**
    * Get the orientation of the object respective to the parallel of the ground (ZX-plane).
    * @return a value typically between n > -90 degrees and n < 90 degrees
    */
  def getPitch : Float = {
    aimPitch
  }

  /**
    * Set the orientation of the object respective to the parallel of the ground (ZX-plane).
    * A value of 0 is perfectly coplanar with the ground (ZX-plane).
    * Compared to *LookSide, where the value is clamped to a circle, here the value is clamped to avoid perfectly vertical orientation.
    * @param look a value clamped between n > -90 degrees and n < 90 degrees
    */
  def setPitch(look : Float) : Unit = {
    var dLook : Float = look
    if(dLook < -89f)
      dLook = -89f
    else if(dLook > 89f)
      dLook = 89f
    aimPitch = dLook
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    PSGameObject.toString(this)
  }
}

object PSGameObject {
  /**
    * A constructor that defines where the entity is and does not need to be invoked with a literal "new."
    * @constructor
    * @param x the x-coordinate
    * @param y the y-coordinate
    * @param z the z-coordinate
    * @return the PSGameObject
    */
  def apply(x : Float, y : Float, z : Float) = {
    new PSGameObject(x, y, z)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : PSGameObject) : String = {
    "(%.2f, %.2f, %.2f)".format(obj.posX, obj.posY, obj.posZ)
  }
}
