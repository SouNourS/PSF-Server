// Copyright (c) 2016 PSForever.net to present
import net.psforever.objects._
import net.psforever.packet.game.{PlanetSideEmpire, PlanetSideGUID}
import org.specs2.mutable._
import net.psforever.types._

class GameObjectTest extends Specification {
  "Game objects" in {
    "PSGameObject" should {
      "constructor (default) / getPosition" in {
        val obj : PSGameObject = new PSGameObject
        val pos : Vector3 = obj.getPosition
        pos.x mustEqual 0f
        pos.y mustEqual 0f
        pos.z mustEqual 0f
      }

      "constructor(Float, Float, Float)" in {
        val test : PSGameObject = new PSGameObject(0.5f, 300.76f, -2f)
        val pos : Vector3 = test.getPosition
        pos.x mustEqual 0.5f
        pos.y mustEqual 300.76f
        pos.z mustEqual -2f
      }

      "setPosition(Float, Float Float)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPosition(0.5f, 300.76f, -2f)
        val pos : Vector3 = obj.getPosition
        pos.x mustEqual 0.5f
        pos.y mustEqual 300.76f
        pos.z mustEqual -2f
      }

      "setPosition(Vector3)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPosition(Vector3(0.5f, 300.76f, -2f))
        val pos : Vector3 = obj.getPosition
        pos.x mustEqual 0.5f
        pos.y mustEqual 300.76f
        pos.z mustEqual -2f
      }

      "getYaw" in {
        val obj : PSGameObject = new PSGameObject
        obj.getYaw mustEqual 0f
      }

      "setYaw (pass)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setYaw(45f)
        obj.getYaw mustEqual 45f
      }

      "setYaw (>=360)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setYaw(361f)
        obj.getYaw mustEqual 1f
      }

      "setYaw (<0)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setYaw(-1f)
        obj.getYaw mustEqual 359f
      }

      "getPitch" in {
        val obj : PSGameObject = new PSGameObject
        obj.getPitch mustEqual 0f
      }

      "setPitch (pass)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPitch(45f)
        obj.getPitch mustEqual 45f
      }

      "setPitch (>89)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPitch(90f)
        obj.getPitch mustEqual 89f
      }

      "setPitch (<-89)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPitch(-90f)
        obj.getPitch mustEqual -89f
      }

      "constructor(Float, Float, Float, Float, Float)" in {
        val test : PSGameObject = new PSGameObject(0.5f, 300.76f, -2f, 45f, 65f)
        val pos : Vector3 = test.getPosition
        pos.x mustEqual 0.5f
        pos.y mustEqual 300.76f
        pos.z mustEqual -2f
        test.getYaw mustEqual 45f
        test.getPitch mustEqual 65f
      }

      "constructor(x, y, z) == obj.setPosition(x, y, z)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPosition(0.5f, 300.76f, -2f)
        val test : PSGameObject = PSGameObject(0.5f, 300.76f, -2f)
        val pos1 : Vector3 = obj.getPosition
        val pos2 : Vector3 = test.getPosition
        pos1.x mustEqual pos2.x
        pos1.y mustEqual pos2.y
        pos1.z mustEqual pos2.z
      }

      "{object}.toString" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPosition(0.5f, 300.76f, -2f)
        obj.toString mustEqual "(0.50, 300.76, -2.00)"
      }

      "PSGameObject.toString(object)" in {
        val obj : PSGameObject = new PSGameObject
        obj.setPosition(0.5f, 300.76f, -2f)
        PSGameObject.toString(obj) mustEqual "(0.50, 300.76, -2.00)"
      }
    }

    "Equipment" should {
      "constructor (default)" in {
        val equipment: Equipment = new Equipment(0)
        equipment.guid mustEqual 0
        equipment.size mustEqual EquipmentSize.BLOCKED
      }

      "constructor(GUID, EquipmentSize)" in {
        val equipment: Equipment = new Equipment(0, EquipmentSize.PISTOL)
        equipment.guid mustEqual 0
        equipment.size mustEqual EquipmentSize.PISTOL
      }

      "construct(GUID, EquipmentSize, Float, Float, Float)" in {
        val equipment: Equipment = Equipment(0, EquipmentSize.PISTOL, 0.5f, 300.76f, -2f)
        val pos : Vector3 = equipment.getPosition
        equipment.guid mustEqual 0
        equipment.size mustEqual EquipmentSize.PISTOL
        pos.x mustEqual 0.5f
        pos.y mustEqual 300.76f
        pos.z mustEqual -2f
      }

      "{object}.toString" in {
        val obj : Equipment = Equipment(3, EquipmentSize.PISTOL, 0.5f, 300.76f, -2f)
        obj.name = "beamer"
        obj.toString mustEqual "{beamer-pos:(0.50, 300.76, -2.00)}"
      }

      "Equipment.toString(object)" in {
        val obj : Equipment = Equipment(3, EquipmentSize.PISTOL, 0.5f, 300.76f, -2f)
        obj.name = "beamer"
        Equipment.toString(obj) mustEqual "{beamer-pos:(0.50, 300.76, -2.00)}"
      }
    }

    "EquipmentSlot" should {
      "constructor (default)" in {
        val slot = new EquipmentSlot
        slot.getSize mustEqual EquipmentSize.BLOCKED
        slot.getEquipment.isDefined mustEqual false
      }

      "constructor(EquipmentSize)" in {
        val slot = new EquipmentSlot(EquipmentSize.PISTOL)
        slot.getSize mustEqual EquipmentSize.PISTOL
        slot.getEquipment.isDefined mustEqual false
      }

      "setSize / getSize" in {
        val slot = new EquipmentSlot
        slot.getSize mustEqual EquipmentSize.BLOCKED
        slot.setSize(EquipmentSize.PISTOL)
        slot.getSize mustEqual EquipmentSize.PISTOL
      }

      "setEquipment / getEquipment (pass)" in {
        val beamer : Equipment = Equipment(0, EquipmentSize.PISTOL)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.getEquipment.isDefined mustEqual false

        val (inserted, discard) = slot.setEquipment(beamer)
        inserted mustEqual true
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq beamer) mustEqual true
        discard.isEmpty mustEqual true
      }

      "setEquipment / getEquipment (fail; equipment wrong size)" in {
        val suppressor : Equipment = Equipment(0, EquipmentSize.RIFLE)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.getEquipment.isDefined mustEqual false

        val (inserted, discard) = slot.setEquipment(suppressor)
        inserted mustEqual false
        slot.getEquipment.isDefined mustEqual false
        discard.isEmpty mustEqual true
      }

      "setSize, dropped wrong-sized equipment on size change" in {
        val beamer : Equipment = Equipment(0, EquipmentSize.PISTOL)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.setEquipment(beamer)
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq beamer) mustEqual true

        val (changed, discard) = slot.setSize(EquipmentSize.RIFLE)
        changed mustEqual true
        slot.getSize mustEqual EquipmentSize.RIFLE
        slot.getEquipment.isDefined mustEqual false
        discard.isDefined mustEqual true
        (discard.get eq beamer) mustEqual true
      }

      "setEquipment (swapped equipment)" in {
        val beamer : Equipment = Equipment(0, EquipmentSize.PISTOL)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.setEquipment(beamer)
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq beamer) mustEqual true

        val amp : Equipment = Equipment(1, EquipmentSize.PISTOL)
        val (inserted, discard) = slot.setEquipment(amp)
        inserted mustEqual true
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq amp) mustEqual true
        discard.isDefined mustEqual true
        (discard.get eq beamer) mustEqual true
      }

      "setEquipment (dropped equipment)" in {
        val beamer : Equipment = Equipment(0, EquipmentSize.PISTOL)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.setEquipment(beamer)
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq beamer) mustEqual true

        val (inserted, discard) = slot.setEquipment(null)
        inserted mustEqual true
        slot.getEquipment.isDefined mustEqual false
        discard.isDefined mustEqual true
        (discard.get eq beamer) mustEqual true
      }

      "setEquipment (fail, blocked by size difference/BLOCKED size)" in {
        val slot : EquipmentSlot = new EquipmentSlot
        slot.getSize mustEqual EquipmentSize.BLOCKED

        val beamer1 = Equipment(0, EquipmentSize.PISTOL)
        val (inserted1, _) = slot.setEquipment(beamer1)
        inserted1 mustEqual false
        slot.getEquipment.isDefined mustEqual false

        val beamer2 = Equipment(0, EquipmentSize.BLOCKED)
        beamer2.size mustEqual EquipmentSize.BLOCKED
        beamer2.size mustEqual slot.getSize //both equipment and slot are same size
        val (inserted2, _) = slot.setEquipment(beamer2)
        inserted2 mustEqual false
        slot.getEquipment.isDefined mustEqual false
      }

      "{object}.toString, empty" in {
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        obj.toString mustEqual "{EquipmentSlot-type:PISTOL}"
      }

      "{object}.toString, equipped" in {
        val beamer : Equipment = new Equipment(3, EquipmentSize.PISTOL, 0.5f, 300.76f, -2f)
        beamer.name = "beamer"
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        obj.setEquipment(beamer)
        obj.toString mustEqual "{EquipmentSlot-type:PISTOL-equipment:{beamer-pos:(0.50, 300.76, -2.00)}}"
      }

      "EquipmentSlot.toString(object), empty" in {
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        EquipmentSlot.toString(obj) mustEqual "{EquipmentSlot-type:PISTOL}"
      }

      "EquipmentSlot.toString(object), equipped" in {
        val beamer : Equipment = new Equipment(3, EquipmentSize.PISTOL, 0.5f, 300.76f, -2f)
        beamer.name = "beamer"
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        obj.setEquipment(beamer)
        EquipmentSlot.toString(obj) mustEqual "{EquipmentSlot-type:PISTOL-equipment:{beamer-pos:(0.50, 300.76, -2.00)}}"
      }
    }

    "ExoSuit" should {
      "constructor (default)" in {
        val suit = new ExoSuit(10)
        suit.guid mustEqual 10
        suit.name mustEqual "exo-suit"
        suit.permission mustEqual 0
        suit.maxArmor mustEqual 0
        suit.inventoryWidth mustEqual 0
        suit.inventoryHeight mustEqual 0
        suit.holsterTypes.length mustEqual 5
        suit.holsterTypes(0) mustEqual EquipmentSize.BLOCKED
        suit.holsterTypes(1) mustEqual EquipmentSize.BLOCKED
        suit.holsterTypes(2) mustEqual EquipmentSize.BLOCKED
        suit.holsterTypes(3) mustEqual EquipmentSize.BLOCKED
        suit.holsterTypes(4) mustEqual EquipmentSize.BLOCKED
      }

      "{object}.toString" in {
        val obj : ExoSuit = new ExoSuit(11)
        obj.name = "newSuit"
        obj.toString mustEqual "{newSuit}"
      }

      "ExoSuit.toString(object)" in {
        val obj : ExoSuit = ExoSuit(11)
        obj.name = "newSuit"
        ExoSuit.toString(obj) mustEqual "{newSuit}"
      }
    }

    "ExoSuitCatalog" should {
      "get standard exo-suit" in {
        val suit : ExoSuit = ExoSuitCatalog.get(0).get
        suit.name mustEqual "standard exo-suit"
        suit.maxArmor mustEqual 50
        suit.inventoryWidth mustEqual 9
        suit.inventoryHeight mustEqual 6
        suit.holsterTypes(0) mustEqual EquipmentSize.PISTOL
        suit.holsterTypes(2) mustEqual EquipmentSize.RIFLE
        suit.holsterTypes(4) mustEqual EquipmentSize.MELEE
      }

      "get agile exo-suit" in {
        val suit : ExoSuit = ExoSuitCatalog.get(1).get
        suit.name mustEqual "agile exo-suit"
        suit.maxArmor mustEqual 100
        suit.inventoryWidth mustEqual 9
        suit.inventoryHeight mustEqual 9
        suit.holsterTypes(0) mustEqual EquipmentSize.PISTOL
        suit.holsterTypes(1) mustEqual EquipmentSize.PISTOL
        suit.holsterTypes(2) mustEqual EquipmentSize.RIFLE
        suit.holsterTypes(4) mustEqual EquipmentSize.MELEE
      }

      "get reinforced exo-suit" in {
        val suit : ExoSuit = ExoSuitCatalog.get(2).get
        suit.name mustEqual "reinforced exo-suit"
        suit.permission mustEqual 1
        suit.maxArmor mustEqual 200
        suit.inventoryWidth mustEqual 12
        suit.inventoryHeight mustEqual 9
        suit.holsterTypes(0) mustEqual EquipmentSize.PISTOL
        suit.holsterTypes(1) mustEqual EquipmentSize.PISTOL
        suit.holsterTypes(2) mustEqual EquipmentSize.RIFLE
        suit.holsterTypes(3) mustEqual EquipmentSize.RIFLE
        suit.holsterTypes(4) mustEqual EquipmentSize.MELEE
      }

      "get infiltration suit" in {
        val suit : ExoSuit = ExoSuitCatalog.get(3).get
        suit.name mustEqual "infiltration suit"
        suit.permission mustEqual 1
        suit.maxArmor mustEqual 0
        suit.inventoryWidth mustEqual 6
        suit.inventoryHeight mustEqual 6
        suit.holsterTypes(0) mustEqual EquipmentSize.PISTOL
        suit.holsterTypes(4) mustEqual EquipmentSize.MELEE
      }

      "get mechanized assault exo-suit" in {
        val suit : ExoSuit = ExoSuitCatalog.get(4).get
        suit.guid mustEqual 4
        suit.name mustEqual "mechanized assault exo-suit"
        suit.permission mustEqual 1
        suit.maxArmor mustEqual 650
        suit.inventoryWidth mustEqual 16
        suit.inventoryHeight mustEqual 16
        suit.holsterTypes(0) mustEqual EquipmentSize.MAX
      }
    }

    "PlayerAvatar" should {
      "constructor (default)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.guid mustEqual 0
        player.name mustEqual "player"
        player.faction mustEqual PlanetSideEmpire.TR
        player.sex mustEqual true
        player.voice mustEqual 0
        player.face mustEqual 0
      }

      "constructor(GUID, String, PlanetSideEmpire, Int, Int)" in {
        val player : PlayerAvatar = PlayerAvatar(2, "Chord", PlanetSideEmpire.VS, false, 2, 3)
        player.guid mustEqual 2
        player.name mustEqual "Chord"
        player.faction mustEqual PlanetSideEmpire.VS
        player.sex mustEqual false
        player.voice mustEqual 3
        player.face mustEqual 2
      }

      "setExoSuitType / getExoSuitType == 'standard exo-suit'" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        ExoSuitCatalog.get(player.getExoSuitType).get.name mustEqual "standard exo-suit"
        player.getPersonalArmor mustEqual 50
        player.getHolster(0).getSize mustEqual EquipmentSize.PISTOL
        player.getHolster(1).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(2).getSize mustEqual EquipmentSize.RIFLE
        player.getHolster(3).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(4).getSize mustEqual EquipmentSize.MELEE
      }

      "setExoSuitType / getExoSuitType == 'agile exo-suit'" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        ExoSuitCatalog.get(player.getExoSuitType).get.name mustEqual "standard exo-suit"
        player.getPersonalArmor mustEqual 50
        player.getHolster(0).getSize mustEqual EquipmentSize.PISTOL
        player.getHolster(1).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(2).getSize mustEqual EquipmentSize.RIFLE
        player.getHolster(3).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(4).getSize mustEqual EquipmentSize.MELEE

        val (changed, _) = player.setExoSuitType(1)
        changed mustEqual true
        ExoSuitCatalog.get(player.getExoSuitType).get.name mustEqual "agile exo-suit"
        player.getPersonalArmor mustEqual 100
        player.getHolster(0).getSize mustEqual EquipmentSize.PISTOL
        player.getHolster(1).getSize mustEqual EquipmentSize.PISTOL
        player.getHolster(2).getSize mustEqual EquipmentSize.RIFLE
        player.getHolster(3).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(4).getSize mustEqual EquipmentSize.MELEE
      }

      "setExoSuitType / getExoSuitType (fails, invalid exo-suit)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        ExoSuitCatalog.get(player.getExoSuitType).get.name mustEqual "standard exo-suit"
        player.getPersonalArmor mustEqual 50
        player.getHolster(0).getSize mustEqual EquipmentSize.PISTOL
        player.getHolster(1).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(2).getSize mustEqual EquipmentSize.RIFLE
        player.getHolster(3).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(4).getSize mustEqual EquipmentSize.MELEE

        val (changed, _) = player.setExoSuitType(100000000)
        changed mustEqual false
        ExoSuitCatalog.get(player.getExoSuitType).get.name mustEqual "standard exo-suit"
        player.getPersonalArmor mustEqual 50
        player.getHolster(0).getSize mustEqual EquipmentSize.PISTOL
        player.getHolster(1).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(2).getSize mustEqual EquipmentSize.RIFLE
        player.getHolster(3).getSize mustEqual EquipmentSize.BLOCKED
        player.getHolster(4).getSize mustEqual EquipmentSize.MELEE
      }

      "getHealth" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.getHealth mustEqual 100
      }

      "setHealth (pass)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setHealth(50)
        player.getHealth mustEqual 50
      }

      "setHealth (too low)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setHealth(-1)
        player.getHealth mustEqual 0
      }

      "setHealth / getMaxHealth (too high)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setHealth(50)
        player.setHealth(101)
        player.getHealth mustEqual 100
        player.getHealth mustEqual player.getMaxHealth
      }

      "getStamina" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.getStamina mustEqual 100
      }

      "setStamina (pass)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setStamina(50)
        player.getStamina mustEqual 50
      }

      "setStamina (too low)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setStamina(-1)
        player.getStamina mustEqual 0
      }

      "setStamina / getMaxStamina (too high)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setStamina(50)
        player.setStamina(101)
        player.getStamina mustEqual 100
        player.getStamina mustEqual player.getMaxStamina
      }

      "getPersonalArmor / getMaxPersonalArmor" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.getPersonalArmor mustEqual 0
        player.getMaxPersonalArmor mustEqual 0
      }

      "setExoSuitType / getExoSuitType" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        player.getExoSuitType mustEqual 0
        player.getPersonalArmor mustEqual 50
        player.getMaxPersonalArmor mustEqual 50
        player.setExoSuitType(1)
        player.getExoSuitType mustEqual 1
        player.getPersonalArmor mustEqual 100
        player.getMaxPersonalArmor mustEqual 100
      }

      "setPersonalArmor (pass)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        player.setPersonalArmor(25)
        player.getPersonalArmor mustEqual 25
      }

      "setPersonalArmor (too low)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setPersonalArmor(-1)
        player.getPersonalArmor mustEqual 0
      }

      "setPersonalArmor / player.getMaxPersonalArmor (too high)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        player.setPersonalArmor(25)
        player.setPersonalArmor(51)
        player.getPersonalArmor mustEqual 50
        player.getPersonalArmor mustEqual player.getMaxPersonalArmor
      }

      "setExoSuitType (personal armor resets)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        player.getPersonalArmor mustEqual 50
        player.setPersonalArmor(25)
        player.getPersonalArmor mustEqual 25
        player.setExoSuitType(1)
        player.getPersonalArmor mustEqual 100
      }

      "setExoSuitType (personal armor unchanged)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        player.getPersonalArmor mustEqual 50
        player.setPersonalArmor(25)
        player.getPersonalArmor mustEqual 25
        player.setExoSuitType(0)
        player.getPersonalArmor mustEqual 25
      }

      "getEquipmentInHolster / setEquipmentInHolster" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.getEquipmentInHolster(0).isDefined mustEqual false

        val (success, _) = player.setEquipmentInHolster(0, beamer)
        success mustEqual true
        player.getEquipmentInHolster(0).isDefined mustEqual true
        (player.getEquipmentInHolster(0).get eq beamer) mustEqual true
      }

      "setEquipmentInHolster (swapped equipment)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)
        player.getHolster(0).getEquipment.isDefined mustEqual true
        (player.getHolster(0).getEquipment.get eq beamer) mustEqual true

        val amp : Equipment = Equipment(2, EquipmentSize.PISTOL)
        val (success, discard) = player.setEquipmentInHolster(0, amp)
        success mustEqual true
        player.getEquipmentInHolster(0).isDefined mustEqual true
        (player.getEquipmentInHolster(0).get eq amp) mustEqual true
        discard.isDefined mustEqual true
        (discard.get eq beamer) mustEqual true
      }

      "setEquipmentInHolster (dropped equipment)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)
        player.getEquipmentInHolster(0).isDefined mustEqual true
        (player.getEquipmentInHolster(0).get eq beamer) mustEqual true

        val (success, discard) = player.setEquipmentInHolster(0, null)
        success mustEqual true
        player.getEquipmentInHolster(0).isDefined mustEqual false
        discard.isDefined mustEqual true
        (discard.get eq beamer) mustEqual true
      }

      "setExoSuitType (dropped wrong-sized equipment)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(1) //agile exo-suit, which has holster 1 for pistols
        val (equipped, _) = player.setEquipmentInHolster(1, beamer)
        equipped mustEqual true
        player.getEquipmentInHolster(1).isDefined mustEqual true

        val (changed, discard) = player.setExoSuitType(0) //standard exo-suit, which blocks holster 1
        changed mustEqual true
        discard.size mustEqual 1
        (discard.head eq beamer) mustEqual true
        player.getEquipmentInHolster(1).isDefined mustEqual false
      }

      "getUsedHolster" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.getUsedHolster mustEqual 255
      }

      "getUsedHolster (no equipment, do not draw holster)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0) //just to make holsters available
        player.getUsedHolster mustEqual 255

        val (drawn, equipment) = player.setUsedHolster(0)
        drawn mustEqual false
        equipment.isDefined mustEqual false
        player.getUsedHolster mustEqual 255
      }

      "setUsedHolster (draw equipped holster)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        val (carry, _) = player.setEquipmentInHolster(0, beamer)
        carry mustEqual true

        val (drawn, equipment) = player.setUsedHolster(0)
        drawn mustEqual true
        equipment.isDefined mustEqual true
        (equipment.get eq beamer) mustEqual true
        player.getUsedHolster mustEqual 0
      }

      "setUsedHolster (put away equipped holster)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)

        player.setUsedHolster(0)
        player.setUsedHolster(255)
        player.getUsedHolster mustEqual 255
      }

      "setUsedHolster (dropped equipment, put away empty holster)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)
        player.setUsedHolster(0)
        player.getUsedHolster mustEqual 0

        val (dropped, equipment) = player.setEquipmentInHolster(0, null)
        dropped mustEqual true
        equipment.isDefined mustEqual true
        (equipment.get eq beamer) mustEqual true
        player.getEquipmentInHolster(0).isDefined mustEqual false
        player.getUsedHolster mustEqual 255
      }

      "setUsedHolster (swapped equipment, put away equipped holster)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        val beamer2 : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)
        player.setUsedHolster(0)

        val (swapped, equipment) = player.setEquipmentInHolster(0, beamer2)
        swapped mustEqual true
        equipment.isDefined mustEqual true
        (equipment.get eq beamer) mustEqual true
        player.getEquipmentInHolster(0).isDefined mustEqual true
        (player.getEquipmentInHolster(0).get eq beamer2) mustEqual true
        player.getUsedHolster mustEqual 255
      }

      "getUsedHolster (exo-suit changes; put away holster)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)
        player.setUsedHolster(0)
        player.getUsedHolster mustEqual 0

        var (changed, dropped) = player.setExoSuitType(1)
        changed mustEqual true
        dropped.size mustEqual 0
        player.getUsedHolster mustEqual 255
      }

      "getUsedHolster (exo-suit changes; put away holster; dropped equipment)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(1)
        player.setEquipmentInHolster(1, beamer)
        player.setUsedHolster(1)
        player.getUsedHolster mustEqual 1

        val (changed, dropped) = player.setExoSuitType(0)
        changed mustEqual true
        dropped.size mustEqual 1
        (dropped.head eq beamer) mustEqual true
        player.getUsedHolster mustEqual 255
      }

      "getUsedHolster (holster is already drawn)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(0)
        player.setEquipmentInHolster(0, beamer)

        var (draw1, equipment1) = player.setUsedHolster(0)
        draw1 mustEqual true
        equipment1.isDefined mustEqual true
        (equipment1.get eq beamer) mustEqual true
        var (draw2, equipment2) = player.setUsedHolster(0)
        draw2 mustEqual false
        equipment2.isDefined mustEqual true
        (equipment2.get eq beamer) mustEqual true
      }

      "getUsedHolster (unequipped but drawn holster; put away holster)" in {
        //this is an error-catching situation that should never come up if programmed correctly
        val player : PlayerAvatar = PlayerAvatar(0)
        val beamer : Equipment = Equipment(1, EquipmentSize.PISTOL)
        player.setExoSuitType(1)
        player.setEquipmentInHolster(1, beamer)
        player.setUsedHolster(1)
        player.getHolster(1).setEquipment(null) // <-- note what we are doing here

        player.getUsedHolster mustEqual 1
        val equipment = player.getEquipmentInHolster(1)
        equipment.isDefined mustEqual false
        player.getUsedHolster mustEqual 255
      }

      "getHolster (not a holster index)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setExoSuitType(0)
        player.getHolster(5) must throwA[IllegalArgumentException]
      }

      "setUsedHolster (not a holster index)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        player.setUsedHolster(5) must throwA[IllegalArgumentException]
      }

      "setHolster (never do this)" in {
        val player : PlayerAvatar = PlayerAvatar(0)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        player.setHolster(0, slot) must throwA[IllegalArgumentException]
      }

      "{object}.toString" in {
        val player : PlayerAvatar = PlayerAvatar(2, "Chord", PlanetSideEmpire.VS, false, 2, 3)
        player.toString mustEqual "{Chord-VS-BR1-CR0}"
      }

      "PlayerAvatar.toString(object)" in {
        val player : PlayerAvatar = PlayerAvatar(2, "Chord", PlanetSideEmpire.VS, false, 2, 3)
        PlayerAvatar.toString(player) mustEqual "{Chord-VS-BR1-CR0}"
      }
    }

    "PlayerMasterList (class)" should {
      //note: we're testing purely the class (instances) and not the object (singleton)
      "addPlayer(PlayerAvatar)" in {
        val masterList = new PlayerMasterList
        val player1 : PlayerAvatar = PlayerAvatar(1)
        masterList.addPlayer(player1) mustEqual true
      }

      "getPlayer(PlanetSideGUID)" in {
        val masterList = new PlayerMasterList
        val id : Int = 0
        val guid : PlanetSideGUID = PlanetSideGUID(id)
        val player1 : PlayerAvatar = PlayerAvatar(id)
        masterList.addPlayer(player1) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual true
        (masterList.getPlayer(guid).get eq player1) mustEqual true
      }

      "getPlayer(PlanetSideGUID) (fail; nothing)" in {
        val masterList = new PlayerMasterList
        var guid : PlanetSideGUID = null
        masterList.getPlayer(guid) mustEqual None
        masterList.getPlayer(0).isDefined mustEqual false
      }

      "getPlayer(Int)" in {
        val masterList = new PlayerMasterList
        val guid : Int = 0
        val player1 : PlayerAvatar = PlayerAvatar(guid)
        masterList.addPlayer(player1) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual true
        (masterList.getPlayer(guid).get eq player1) mustEqual true
      }

      "removePlayer(PlayerAvatar)" in {
        val masterList = new PlayerMasterList
        val id : Int = 0
        val guid : PlanetSideGUID = PlanetSideGUID(id)
        val player1 : PlayerAvatar = PlayerAvatar(id)
        masterList.addPlayer(player1) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual true
        masterList.removePlayer(player1) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual false
      }

      "removePlayer(PlanetSideGUID)" in {
        val masterList = new PlayerMasterList
        val id : Int = 0
        val guid : PlanetSideGUID = PlanetSideGUID(id)
        val player1 : PlayerAvatar = PlayerAvatar(id)
        masterList.addPlayer(player1) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual true
        masterList.removePlayer(guid) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual false
      }

      "removePlayer(PlanetSideGUID) (fail on null)" in {
        val masterList = new PlayerMasterList
        val guid : PlanetSideGUID = null
        masterList.removePlayer(guid) mustEqual false
      }

      "removePlayer(Int)" in {
        val masterList = new PlayerMasterList
        val guid : Int = 0
        val player1 : PlayerAvatar = PlayerAvatar(guid)
        masterList.addPlayer(player1) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual true
        masterList.removePlayer(guid) mustEqual true
        masterList.getPlayer(guid).isDefined mustEqual false
      }

      "removePlayer(Int) (fail; nothing)" in {
        val masterList = new PlayerMasterList
        masterList.removePlayer(0) mustEqual false
      }

      "shutdown (remove all players)" in {
        val masterList = new PlayerMasterList
        masterList.addPlayer( PlayerAvatar(1) ) mustEqual true
        masterList.addPlayer( PlayerAvatar(2) ) mustEqual true
        masterList.addPlayer( PlayerAvatar(3) ) mustEqual true
        masterList.getPlayer(1).isDefined mustEqual true
        masterList.getPlayer(2).isDefined mustEqual true
        masterList.getPlayer(3).isDefined mustEqual true
        masterList.shutdown
        masterList.getPlayer(1).isDefined mustEqual false
        masterList.getPlayer(2).isDefined mustEqual false
        masterList.getPlayer(3).isDefined mustEqual false
      }

      "shutdown (output)" in {
        val masterList = new PlayerMasterList
        var player : PlayerAvatar = PlayerAvatar(1)
        masterList.addPlayer(player) mustEqual true
        masterList.getPlayer(1).isDefined mustEqual true
        var list : List[PlayerAvatar] = masterList.shutdown
        masterList.getPlayer(1).isDefined mustEqual false
        (list.head eq player) mustEqual true
      }

      "addPlayer (fail; repeated player guid)" in {
        val masterList = new PlayerMasterList
        val guid : Int = 0
        val player1: PlayerAvatar = PlayerAvatar(guid)
        val player2: PlayerAvatar = PlayerAvatar(guid)
        masterList.addPlayer(player1) mustEqual true
        masterList.addPlayer(player2) mustEqual false
        masterList.getPlayer(guid).isDefined mustEqual true
        (masterList.getPlayer(guid).get eq player1) mustEqual true
      }

      "addPlayer(PlayerAvatar, Long)" in {
        val masterList = new PlayerMasterList
        val external : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(0)
        masterList.addPlayer(player1, external) mustEqual true
      }

      "addPlayer(PlayerAvatar, Long) (entry already found)" in {
        val masterList = new PlayerMasterList
        val external : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(0)
        masterList.addPlayer(player1, external) mustEqual true
        masterList.addPlayer(player1, external) mustEqual true // does something different than the first time
      }

      "getPlayer(Long)" in {
        val masterList = new PlayerMasterList
        val id : Int = 0
        val external : Long = 1L
        val guid : PlanetSideGUID = PlanetSideGUID(id)
        val player1 : PlayerAvatar = PlayerAvatar(id)
        masterList.addPlayer(player1, external) mustEqual true

        val playerA : Option[PlayerAvatar] = masterList.getPlayer(guid)
        val playerB : Option[PlayerAvatar] = masterList.getPlayer(external)
        playerA.isDefined mustEqual true
        playerB.isDefined mustEqual true
        (playerA.get eq playerB.get) mustEqual true
      }

      "removePlayer(Long) (pass)" in {
        val masterList = new PlayerMasterList
        val id : Int = 0
        val external : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id)
        masterList.addPlayer(player1, external) mustEqual true

        masterList.getPlayer(external).isDefined mustEqual true
        masterList.removePlayer(external)
        masterList.getPlayer(external).isDefined mustEqual false
        masterList.getPlayer(id).isDefined mustEqual false
      }

      "removePlayer(Long) (pass; remove external id at same time)" in {
        val masterList = new PlayerMasterList
        val id : Int = 0
        val external : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id)
        masterList.addPlayer(player1, external) mustEqual true

        masterList.getPlayer(id).isDefined mustEqual true
        masterList.getPlayer(external).isDefined mustEqual true
        masterList.removePlayer(id)
        masterList.getPlayer(id).isDefined mustEqual false
        masterList.getPlayer(external).isDefined mustEqual false
      }

      "removePlayer(Long) (fail; guid doesn't exist)" in {
        val masterList = new PlayerMasterList
        masterList.removePlayer(0) mustEqual false //because the
      }

      "removePlayer(Long) (fail; external id doesn't exist)" in {
        val masterList = new PlayerMasterList
        masterList.removePlayer(5L) mustEqual false
      }

      "getPlayer(Int, Long) (pass and fail)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        val id2 : Int = 3
        val external2 : Long = 2L
        val player2 : PlayerAvatar = PlayerAvatar(id2)
        masterList.addPlayer(player1, external1) mustEqual true
        masterList.addPlayer(player2, external2) mustEqual true

        masterList.getPlayer(id1, external1).isDefined mustEqual true
        masterList.getPlayer(id2, external2).isDefined mustEqual true
        masterList.getPlayer(id1, external2).isDefined mustEqual false
        masterList.getPlayer(id2, external1).isDefined mustEqual false
      }

      "addPlayer (fail; repeated external id)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        val id2 : Int = 3
        val player2 : PlayerAvatar = PlayerAvatar(id2)
        masterList.addPlayer(player1, external1) mustEqual true
        masterList.addPlayer(player2, external1) mustEqual false
        (masterList.getPlayer(external1).get eq player1) mustEqual true
        masterList.getPlayer(id2).isDefined mustEqual false
      }

      "addPlayer (fail; trying to overwrite external id)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        var external2 : Long = 2L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        masterList.addPlayer(player1, external1) mustEqual true
        masterList.addPlayer(player1, external2) mustEqual false
        (masterList.getPlayer(external1).get eq player1) mustEqual true
        masterList.getPlayer(external2).isDefined mustEqual false
      }

      "userClaimsCharacter" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        masterList.addPlayer(player1) mustEqual true

        masterList.getPlayer(external1).isDefined mustEqual false
        masterList.userClaimsCharacter(external1, id1)
        masterList.getPlayer(external1).isDefined mustEqual true
        (masterList.getPlayer(external1).get eq player1) mustEqual true
      }

      "userDissociatesCharacter (via addPlayer)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        masterList.addPlayer(player1, external1) mustEqual true

        masterList.getPlayer(external1).isDefined mustEqual true
        (masterList.getPlayer(external1).get eq player1) mustEqual true
        masterList.userDissociatesCharacter(external1)
        masterList.getPlayer(external1).isDefined mustEqual false
        (masterList.getPlayer(id1).get eq player1) mustEqual true // still there
      }

      "userDissociatesCharacter (via userClaimsCharacter)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        masterList.addPlayer(player1) mustEqual true
        masterList.userClaimsCharacter(external1, id1) mustEqual id1

        masterList.getPlayer(external1).isDefined mustEqual true
        (masterList.getPlayer(external1).get eq player1) mustEqual true
        masterList.userDissociatesCharacter(external1)
        masterList.getPlayer(external1).isDefined mustEqual false
        (masterList.getPlayer(id1).get eq player1) mustEqual true // still there
      }

      "userClaimsCharacter (fails; nothing)" in {
        val masterList = new PlayerMasterList
        masterList.userClaimsCharacter(0L, 0) mustEqual Int.MinValue
      }

      "userClaimsCharacter (fail; repeated external id)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        val id2 : Int = 3
        val player2 : PlayerAvatar = PlayerAvatar(id2)
        masterList.addPlayer(player1, external1) mustEqual true
        masterList.addPlayer(player2) mustEqual true

        masterList.userClaimsCharacter(external1, id2) mustEqual Int.MinValue
        (masterList.getPlayer(external1).get eq player1) mustEqual true
        masterList.getPlayer(id2, external1).isDefined mustEqual false
      }

      "userClaimsCharacter (fail; trying to overwrite external id)" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 1L
        var external2 : Long = 2L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        masterList.addPlayer(player1, external1) mustEqual true

        masterList.userClaimsCharacter(external2, id1)
        (masterList.getPlayer(external1).get eq player1) mustEqual true
        masterList.getPlayer(external2).isDefined mustEqual false
      }

      "userDissociatesCharacter (fails; nothing)" in {
        val masterList = new PlayerMasterList
        masterList.userDissociatesCharacter(0L) mustEqual Int.MinValue
        masterList.userDissociatesCharacter(1L, 1) mustEqual Int.MinValue
      }

      "getUnclaimedCharacters" in {
        val masterList = new PlayerMasterList
        val id1 : Int = 0
        val external1 : Long = 10L
        val player1 : PlayerAvatar = PlayerAvatar(id1)
        val id2 : Int = 1
        val player2 : PlayerAvatar = PlayerAvatar(id2)
        val id3 : Int = 2
        val external3 : Long = 20L
        val player3 : PlayerAvatar = PlayerAvatar(id3)
        masterList.addPlayer(player1, external1) mustEqual true
        masterList.addPlayer(player2) mustEqual true
        masterList.addPlayer(player3, external3) mustEqual true

        masterList.getUnclaimedCharacters.size mustEqual 1
        masterList.getUnclaimedCharacters.head mustEqual id2
        masterList.userDissociatesCharacter(external1)
        masterList.getUnclaimedCharacters.size mustEqual 2
      }

      "getWorldPopulation" in {
        val masterList = new PlayerMasterList
        masterList.addPlayer( PlayerAvatar(0, "s1", PlanetSideEmpire.VS, false, 2, 3) )
        masterList.addPlayer( PlayerAvatar(1, "s2", PlanetSideEmpire.VS, false, 2, 3) )
        masterList.addPlayer( PlayerAvatar(2, "s3", PlanetSideEmpire.TR, false, 2, 3) )
        masterList.addPlayer( PlayerAvatar(3, "s4", PlanetSideEmpire.TR, false, 2, 3) )
        masterList.addPlayer( PlayerAvatar(4, "s5", PlanetSideEmpire.TR, false, 2, 3) )
        masterList.addPlayer( PlayerAvatar(5, "s6", PlanetSideEmpire.NC, false, 2, 3) )
        val (nc, tr, vs) = masterList.getWorldPopulation
        nc mustEqual 1
        tr mustEqual 3
        vs mustEqual 2
      }

      "started" in {
        // Check when this instance was created
        val masterList = new PlayerMasterList
        masterList.started > 0L // Unix time
      }
    }

    "PlayerMasterList (object)" should {
      // Due to the way the testing suite handles concurrency and singletons, every test must be executed in one long block.
      // The good news is that, since the object calls a singular instance of the instance, one merely has to run through all of the functions once.
      "everything" in {
        val id1 : Int = 0
        val guid1 : PlanetSideGUID = PlanetSideGUID(id1)
        val id2 : Int = 1
        val id3 : Int = 2
        val guid3 : PlanetSideGUID = PlanetSideGUID(id3)
        val id4 : Int = 3
        val player1 : PlayerAvatar = PlayerAvatar(id1, "s1", PlanetSideEmpire.VS, false, 2, 3)
        val player2 : PlayerAvatar = PlayerAvatar(id2, "s2", PlanetSideEmpire.TR, false, 2, 3)
        val player3 : PlayerAvatar = PlayerAvatar(id3, "s3", PlanetSideEmpire.NC, false, 2, 3)
        val player4 : PlayerAvatar = PlayerAvatar(id4, "s4", PlanetSideEmpire.TR, false, 2, 3)
        val externId2 : Long = 20L
        val externId4 : Long = 40L

        PlayerMasterList.addPlayer(player1) mustEqual true // addPlayer(PlayerAvatar)
        PlayerMasterList.getPlayer(id1).isDefined mustEqual true // getPlayer(Int)
        PlayerMasterList.getPlayer(guid1).isDefined mustEqual true // getPlayer(PlanetSideGUID)
        PlayerMasterList.addPlayer(player2, externId2) mustEqual true // addPlayer(PlayerAvatar, Long)
        PlayerMasterList.getPlayer(externId2).isDefined mustEqual true // getPlayer(Long)
        PlayerMasterList.getPlayer(id2, externId2).isDefined mustEqual true // getPlayer(Int, Long)
        PlayerMasterList.addPlayer(player3) mustEqual true
        PlayerMasterList.getWorldPopulation //getWorldPopulation
        PlayerMasterList.getUnclaimedCharacters //getUnclaimedCharacters
        PlayerMasterList.removePlayer(id1) mustEqual true // removePlayer(Int)
        PlayerMasterList.removePlayer(externId2) mustEqual true // removePlayer(Long)
        PlayerMasterList.removePlayer(guid3) mustEqual true // removePlayer(PlanetSideGUID)
        PlayerMasterList.addPlayer(player4) mustEqual true
        PlayerMasterList.userClaimsCharacter(externId4, id4) mustEqual id4 // userClaimsCharacter(Long, Int)
        PlayerMasterList.userDissociatesCharacter(externId4) mustEqual id4 // userDissociatesCharacter(Long)
        PlayerMasterList.userClaimsCharacter(externId4, id4) mustEqual id4
        PlayerMasterList.userDissociatesCharacter(externId4, id4) mustEqual id4 // userDissociatesCharacter(Long, Int)
        PlayerMasterList.userClaimsCharacter(externId4, id4) mustEqual id4
        PlayerMasterList.removePlayer(player4) mustEqual true // removePlayer(PlayerAvatar)
        PlayerMasterList.shutdown.isEmpty mustEqual true // shutdown
      }
    }
  }
}
