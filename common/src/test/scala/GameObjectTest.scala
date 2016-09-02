// Copyright (c) 2016 PSForever.net to present
import net.psforever.objects._
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
        val equipment: Equipment = Equipment(0)
        equipment.guid mustEqual 0
        equipment.getSize mustEqual EquipmentSize.BLOCKED
      }

      "construct(GUID, EquipmentSize, Float, Float, Float)" in {
        val equipment: Equipment = Equipment(0, 0.5f, 300.76f, -2f)
        val pos : Vector3 = equipment.getPosition
        equipment.guid mustEqual 0
        pos.x mustEqual 0.5f
        pos.y mustEqual 300.76f
        pos.z mustEqual -2f
      }

      "getName" in {
        val equipment: Equipment = Equipment(0)
        equipment.getName mustEqual "Equipment"
      }

      "setName" in {
        val equipment: Equipment = Equipment(0)
        equipment.setName("beamer") //does nothing
        equipment.getName mustEqual "Equipment"
      }

      "{object}.toString" in {
        val obj : Equipment = Equipment(3, 0.5f, 300.76f, -2f)
        obj.toString mustEqual "{Equipment}"
      }

      "Equipment.toString(object)" in {
        val obj : Equipment = Equipment(3, 0.5f, 300.76f, -2f)
        Equipment.toString(obj) mustEqual "{Equipment}"
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
        val beamer : Tool = Tool(0, 0)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.getEquipment.isDefined mustEqual false

        val (inserted, discard) = slot.setEquipment(beamer)
        inserted mustEqual true
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq beamer) mustEqual true
        discard.isEmpty mustEqual true
      }

      "setEquipment / getEquipment (fail; equipment wrong size)" in {
        val suppressor : Tool = Tool(0, 1)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.getEquipment.isDefined mustEqual false

        val (inserted, discard) = slot.setEquipment(suppressor)
        inserted mustEqual false
        slot.getEquipment.isDefined mustEqual false
        discard.isEmpty mustEqual true
      }

      "setSize, dropped wrong-sized equipment on size change" in {
        val beamer : Tool = Tool(0, 0)
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
        val beamer : Tool = Tool(0, 0)
        val slot : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        slot.setEquipment(beamer)
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq beamer) mustEqual true

        val amp : Tool = Tool(1, 3)
        val (inserted, discard) = slot.setEquipment(amp)
        inserted mustEqual true
        slot.getEquipment.isDefined mustEqual true
        (slot.getEquipment.get eq amp) mustEqual true
        discard.isDefined mustEqual true
        (discard.get eq beamer) mustEqual true
      }

      "setEquipment (dropped equipment)" in {
        val beamer : Tool = Tool(0, 0)
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

        val beamer1 : Tool = Tool(0, 0)
        val (inserted1, _) = slot.setEquipment(beamer1)
        inserted1 mustEqual false
        slot.getEquipment.isDefined mustEqual false
      }

      "{object}.toString, empty" in {
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        obj.toString mustEqual "{EquipmentSlot-type:PISTOL}"
      }

      "{object}.toString, equipped" in {
        val beamer : Tool = Tool(3, 0, 0.5f, 300.76f, -2f)
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        obj.setEquipment(beamer)
        obj.toString mustEqual "{EquipmentSlot-type:PISTOL-equipment:{beamer-CELL_ENERGY(0/16)-fire:0}}"
      }

      "EquipmentSlot.toString(object), empty" in {
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        EquipmentSlot.toString(obj) mustEqual "{EquipmentSlot-type:PISTOL}"
      }

      "EquipmentSlot.toString(object), equipped" in {
        val beamer : Tool = new Tool(3, 0, 0.5f, 300.76f, -2f)
        val obj : EquipmentSlot = EquipmentSlot(EquipmentSize.PISTOL)
        obj.setEquipment(beamer)
        EquipmentSlot.toString(obj) mustEqual "{EquipmentSlot-type:PISTOL-equipment:{beamer-CELL_ENERGY(0/16)-fire:0}}"
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

    "InventoryItem" should {
      "constructor" in {
        val beamer = Tool(0, 0)
        val invItem : InventoryItem = InventoryItem(beamer, 2, 5)
        (invItem.obj eq beamer) mustEqual true
        invItem.y mustEqual 2
        invItem.x mustEqual 5
      }

      "constructor (failure)" in {
        InventoryItem(null, 2, 5) must throwA[IllegalArgumentException]
      }

      "getInventorySize" in {
        val beamer = Tool(0, 0)
        val invItem : InventoryItem = InventoryItem(beamer, 2, 5)
        val invSize : (Int, Int) = invItem.getInventorySize
        val beamerSize : (Int, Int) = beamer.getInventorySize
        beamerSize._1 mustEqual invSize._1
        beamerSize._2 mustEqual invSize._2
      }

      "{object}.toString" in {
        val beamer = Tool(0, 0)
        val obj : InventoryItem = InventoryItem(beamer, 2, 5)
        obj.toString mustEqual "<{beamer-CELL_ENERGY(0/16)-fire:0}>"
      }

      "InventoryItem.toString(object)" in {
        val beamer = Tool(0, 0)
        val obj : InventoryItem = InventoryItem(beamer, 2, 5)
        InventoryItem.toString(obj) mustEqual "<{beamer-CELL_ENERGY(0/16)-fire:0}>"
      }
    }
  }
}
