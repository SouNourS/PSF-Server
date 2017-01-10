// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game.objectcreate

import scodec.{Attempt, Codec, Err}
import scodec.codecs._

import scala.annotation.switch

/**
  * A reference between all object class codes and the name of the object they represent.<br>
  * <br>
  * Object classes compose a number between 0 and (probably) 2047, always translating into an 11-bit value.
  * They are recorded as little-endian hexadecimal values here.
  * In `scodec` terms, that's a `uintL(11)` or `uintL(0xB)`.
  */
object ObjectClass {
  //character
  final val AVATAR = 0x79 // 121
  //ammunition
  final val BULLETS_9MM = 0x1C // 28
  final val BULLETS_9MM_AP = 0x1D // 29
  final val ENERGY_CELL = 0x110 // 272
  final val JAMMER_GRENADE_AMMO = 0x1A1 // 417
  final val FORCE_BLADE_AMMO = 0x21C // 540
  final val PLASMA_GRENADE_AMMO = 0x2A9 // 681
  final val BUCKSHOT = 0x2F3 // 755 //TODO apply internal name, eventually
  final val STRIKER_MISSILE = 0x347 // 839
  final val ARMOR_CANISTER = 0x6f // 111
  final val INCENDIARY_GEL = 0x12c // 300
  final val HEALTH_CANISTER = 0x185 // 389
  final val BOLT = 0x91 // 145
  final val FRAGMENTATION_GRENADE_AMMO = 0x14b // 331
  final val ROTARY_CHAINGUN_BULLETS_12MM = 0x3 // 3
  final val TANK_SHELL_150MM = 0x6
  final val CHAINGUN_BULLETS_15MM = 0x9
  final val RECOILLESS_CANON_BULLETS_20MM = 0x10 // 16
  final val LIBERATOR_VULTURE_TAILGUN_BULLETS_25MM = 0x13 // 19
  final val CHAINGUN_BULLETS_35MM = 0x15 // 21
  final val LIGHTNING_SHELL_75MM = 0x19 // 25
  final val ANCIENT_CAPACITOR_UNIT = 0x32 // 50
  final val MULTI_PHASE_10mm = 0x36 // 54
  final val APHELION_IMMOLATION_CANNON_AMMO = 0x56 // 86
  final val APHELION_CONTINUOUS_LASER_BATTERY = 0x59 // 89
  final val APHELION_PLASMA_PROJECTOR_AMMO = 0x61 // 97
  final val APHELION_PPR_AMMO = 0x65 // 101
  final val APHELION_STARFIRE_AMMO = 0x6a // 106
  //weapons
  final val SUPPRESSOR = 0x34D // 845
  final val BEAMER = 0x8C // 140
  final val SWEEPER = 0x130 // 304
  final val FORCE_BLADE = 0x144 // 324
  final val GAUSS = 0x159 // 345
  final val JAMMER_GRENADE = 0x1A0 // 416
  final val PLASMA_GRENADE = 0x2A8 // 680
  final val MCG = 0x22c // 556
  final val CHAIN_BLADE = 0xaf // 175
  final val PUNISHER = 0x2c2 // 706
  final val DRAGON = 0x12b // 299
  final val BOLT_DRIVER = 0x92 // 146
  final val PULSAR = 0x2bd // 701
  final val FRAGMENTATION_GRENADE = 0x14a // 330
  final val SCATTER_PISTOL = 0x19b // 411
  final val SPEAR = 0x37 // 55
  final val STINGER = 0x38 // 56
  final val ERASER = 0x39 // 57
  //tools
  final val MEDKIT = 0x218 // 536
  final val REK = 0x2D8 // 728
  final val MEDICAL_APPLICATOR = 0x213 // 531
  final val BANK = 0x84 // 132
  final val NANO_DISPENSER = 0x241 // 577
  final val ACE = 0x20 // 32
  final val CUD = 0xd5 // 213
  final val FDU = 0x27 // 39
  //unknown
  final val SLOT_BLOCKER = 0x1C8 // 456 //strange item found in inventory slot #5, between holsters and grid

  // hexa
  // for later e f 18 28 45 46 47 48 49 4a 4b 4c 4d
  // 4e 77 78 88 89 8a 8b
  // decimal
  // 46 = AMS
  // 52 = Door
  // 60 = ANT
  // 62 = sundy
  // 66 = vindicator
  // 67 = juggernaut
  // 68 = leviathan
  // 110 = applicator ?!
  // 112 = armor siphon ammo ?!
  // 118 = aurora
  // 135 = raider

  // test en cours
  final val TEMP1 = 0x83 //
  final val TEMP2 = 0x84 //
  final val TEMP3 = 0x85 //
  final val TEMP4 = 0x86 //
  final val TEMP5 = 0x87 //
  final val TEMP6 = 0x88 //
  final val TEMP7 = 0x89 //
  final val TEMP8 = 0x8a //
  final val TEMP9 = 0x8b //
  final val TEMP0 = 0x8c //

  //TODO refactor this function into another object later
  /**
    * Given an object class, retrieve the `Codec` used to parse and translate the constructor data for that type.<br>
    * <br>
    * This function serves as a giant `switch` statement that loosely connects object data to object class.
    * All entries, save the default, merely point to the `Codec` of pattern `ConstructorData.genericPattern`.
    * This pattern connects all `Codec`s back to the superclass `ConstructorData`.
    * The default case is a failure case for trying to either decode or encode an unknown class of object.
    * @param objClass the code for the type of object being constructed
    * @return the `Codec` that handles the format of data for that particular item class, or a failing `Codec`
    */
  def selectDataCodec(objClass : Int) : Codec[ConstructorData.genericPattern] = {
    (objClass : @switch) match {
      case ObjectClass.AVATAR => CharacterData.genericCodec
      case ObjectClass.BEAMER => WeaponData.genericCodec
      case ObjectClass.BUCKSHOT => AmmoBoxData.genericCodec
      case ObjectClass.BULLETS_9MM => AmmoBoxData.genericCodec
      case ObjectClass.BULLETS_9MM_AP => AmmoBoxData.genericCodec
      case ObjectClass.ENERGY_CELL => AmmoBoxData.genericCodec
      case ObjectClass.FORCE_BLADE_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.FORCE_BLADE => WeaponData.genericCodec
      case ObjectClass.GAUSS => WeaponData.genericCodec
      case ObjectClass.JAMMER_GRENADE => WeaponData.genericCodec
      case ObjectClass.JAMMER_GRENADE_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.MEDKIT => AmmoBoxData.genericCodec
      case ObjectClass.PLASMA_GRENADE => WeaponData.genericCodec
      case ObjectClass.PLASMA_GRENADE_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.REK => REKData.genericCodec
      case ObjectClass.SLOT_BLOCKER => AmmoBoxData.genericCodec
      case ObjectClass.SUPPRESSOR => WeaponData.genericCodec
      case ObjectClass.SWEEPER => WeaponData.genericCodec
      case ObjectClass.BANK => WeaponData.genericCodec // TODO maybe tools, not weapon
      case ObjectClass.MEDICAL_APPLICATOR => WeaponData.genericCodec // TODO maybe tools, not weapon
      case ObjectClass.MCG => WeaponData.genericCodec
      case ObjectClass.STRIKER_MISSILE => AmmoBoxData.genericCodec
      case ObjectClass.ARMOR_CANISTER => AmmoBoxData.genericCodec
      case ObjectClass.CHAIN_BLADE => WeaponData.genericCodec
      case ObjectClass.PUNISHER => WeaponData.genericCodec
      case ObjectClass.NANO_DISPENSER => WeaponData.genericCodec // TODO maybe tools, not weapon
      case ObjectClass.DRAGON => WeaponData.genericCodec
      case ObjectClass.BOLT_DRIVER => WeaponData.genericCodec
      case ObjectClass.INCENDIARY_GEL => AmmoBoxData.genericCodec
      case ObjectClass.HEALTH_CANISTER => AmmoBoxData.genericCodec
      case ObjectClass.BOLT => AmmoBoxData.genericCodec
      case ObjectClass.PULSAR => WeaponData.genericCodec
      case ObjectClass.ACE => WeaponData.genericCodec // TODO maybe tools, not weapon
      case ObjectClass.FRAGMENTATION_GRENADE => WeaponData.genericCodec
      case ObjectClass.FRAGMENTATION_GRENADE_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.SCATTER_PISTOL => WeaponData.genericCodec
      case ObjectClass.CUD => WeaponData.genericCodec // TODO maybe tools, not weapon
      case ObjectClass.ROTARY_CHAINGUN_BULLETS_12MM => AmmoBoxData.genericCodec
      case ObjectClass.CHAINGUN_BULLETS_15MM => AmmoBoxData.genericCodec
      case ObjectClass.TANK_SHELL_150MM => AmmoBoxData.genericCodec
      case ObjectClass.RECOILLESS_CANON_BULLETS_20MM => AmmoBoxData.genericCodec
      case ObjectClass.LIBERATOR_VULTURE_TAILGUN_BULLETS_25MM => AmmoBoxData.genericCodec
      case ObjectClass.CHAINGUN_BULLETS_35MM => AmmoBoxData.genericCodec
      case ObjectClass.LIGHTNING_SHELL_75MM => AmmoBoxData.genericCodec
      case ObjectClass.FDU => WeaponData.genericCodec // TODO maybe tools, not weapon
      case ObjectClass.ANCIENT_CAPACITOR_UNIT => AmmoBoxData.genericCodec
      case ObjectClass.MULTI_PHASE_10mm => AmmoBoxData.genericCodec
      case ObjectClass.SPEAR => WeaponData.genericCodec
      case ObjectClass.STINGER => WeaponData.genericCodec
      case ObjectClass.ERASER => WeaponData.genericCodec
      case ObjectClass.APHELION_IMMOLATION_CANNON_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.APHELION_CONTINUOUS_LASER_BATTERY => AmmoBoxData.genericCodec
      case ObjectClass.APHELION_PLASMA_PROJECTOR_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.APHELION_PPR_AMMO => AmmoBoxData.genericCodec
      case ObjectClass.APHELION_STARFIRE_AMMO => AmmoBoxData.genericCodec


      case ObjectClass.TEMP1 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP2 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP3 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP4 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP5 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP6 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP7 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP8 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP9 => WeaponData.genericCodec // TODO remove temp
      case ObjectClass.TEMP0 => WeaponData.genericCodec // TODO remove temp
              //failure case
      case _ => conditional(false, bool).exmap[ConstructorData.genericPattern] (
        {
          case None | _ =>
            Attempt.failure(Err("decoding unknown object class"))
        },
        {
          case None | _ =>
            Attempt.failure(Err("encoding unknown object class"))
        }
      )
    }
  }
}
