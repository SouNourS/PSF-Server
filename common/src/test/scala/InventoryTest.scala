// Copyright (c) 2016 PSForever.net to present
import net.psforever.objects._
import org.specs2.mutable._

class InventoryTest extends Specification {
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

  "Inventory" should {
    val inv : ListInventory = ListInventory(12, 9)

    "offset" in {
      inv.offset mustEqual 0
    }

    "toHexIndex" in {
      Inventory.toHexIndex(inv, 0, 0) mustEqual 0
      Inventory.toHexIndex(inv, 1, 0) mustEqual 12
      Inventory.toHexIndex(inv, 1, 3) mustEqual 15
    }

    "toHexIndex (fail, negative coordinates)" in {
      Inventory.toHexIndex(inv, -1, 0) mustEqual -1
      Inventory.toHexIndex(inv, 0, -1) mustEqual -1
    }

    "toHexIndex (fail, coordinates too high)" in {
      Inventory.toHexIndex(inv, 9, 0) mustEqual -1
      Inventory.toHexIndex(inv, 0, 12) mustEqual -1
    }

    "fromHexIndex" in {
      Inventory.fromHexIndex(inv, 0) mustEqual (0, 0)
      Inventory.fromHexIndex(inv, 12) mustEqual (1, 0)
      Inventory.fromHexIndex(inv, 15) mustEqual (1, 3)
    }

    "fromHexIndex (fail, index too low)" in {
      Inventory.fromHexIndex(inv, -1) mustEqual (-1, -1)
    }

    "fromHexIndex (fail, index too high)" in {
      Inventory.fromHexIndex(inv, 109) mustEqual (-1, -1)
    }
  }

  "ListInventory" should {
    "constructor" in {
      val inv : ListInventory = ListInventory(12, 9)
      inv.width mustEqual 12
      inv.height mustEqual 9
    }

    "get capacity" in {
      val inv : ListInventory = ListInventory(12, 9)
      inv.capacity mustEqual (12 * 9)
    }

    "get size" in {
      val inv : ListInventory = ListInventory(12, 9)
      inv.size mustEqual 0
    }

    "addItem (cartesian coordinates)" in {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, swapped : Option[Equipment]) = inv.addItem(beamer, 0,0)
      success mustEqual true
      swapped.isDefined mustEqual false
      inv.capacity mustEqual (12 * 9) //unchanged
      inv.size mustEqual 1
    }

    "addItem (hex coordinate)" in {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, swapped : Option[Equipment]) = inv.addItem(beamer, 0)
      success mustEqual true
      swapped.isDefined mustEqual false
      inv.capacity mustEqual (12 * 9) //unchanged
      inv.size mustEqual 1
    }

    "addItem (swap with overlapping item)" in {
      val beamer1 : Tool = Tool(0, 0)
      val beamer2 : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer1, 0, 0)
      inv.size mustEqual 1
      val (success : Boolean, swapped : Option[Equipment]) = inv.addItem(beamer2, 2,2) //only overlapping one square in the corner
      inv.size mustEqual 1 //same
      success mustEqual true
      swapped.isDefined mustEqual true
      (swapped.get eq beamer1) mustEqual true //beamer1 was removed
      inv.getItem(10).isDefined mustEqual true //found beamer2
    }

    "addItem (fail; single coordinate wrong)" in {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, -1)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; x-coordinate too low) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, 0, -1)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; x-coordinate too high) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, 0, 13)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; y-coordinate too low) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, -1, 0)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; y-coordinate too high) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, 10, 0)
      success mustEqual false
      inv.size mustEqual 0
    }

    //all coordinates should check against the size of the inventory; only add tests for that for failing cases from now on

    "addItem (fail; inserting same item twice" in {
      val beamer : Tool = Tool(0, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success1 : Boolean, _) = inv.addItem(beamer, 0, 0)
      success1 mustEqual true
      val (success2 : Boolean, _) = inv.addItem(beamer, 3, 0)
      success2 mustEqual false
    }

    "addItem (fail; GUID already encountered?!)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      val (success1 : Boolean, _) = inv.addItem(beamer1, 0, 0)
      success1 mustEqual true
      val (success2 : Boolean, _) = inv.addItem(beamer2, 3, 0)
      success2 mustEqual false
    }

    "addItem (fail; no swap when two or more existing items overlap)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(20, 0)
      val beamer3 : Tool = Tool(30, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer1, 0, 0)
      inv.addItem(beamer2, 4, 0)
      inv.size mustEqual 2

      val (success : Boolean, value : Option[Equipment]) = inv.addItem(beamer3, 2, 2)
      inv.size mustEqual 2 //unchanged
      success mustEqual false
      value.isDefined mustEqual false
      inv.getItem(beamer3).isDefined mustEqual false

    }

    "getItem (guid)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val item : Option[Equipment] = inv.getItem(50)
      inv.size mustEqual 1
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true
    }

    "getItem (equipment itself)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val item : Option[Equipment] = inv.getItem(beamer)
      inv.size mustEqual 1
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true
    }

    "getItem (cartesian coordinates)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      var item : Option[Equipment] = None

      item = inv.getItem(1, 3) //upper left corner
      inv.size mustEqual 1
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true

      item = inv.getItem(2, 4) //middle
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true

      item = inv.getItem(3, 5) //lower right corner
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true
    }

    "getItem (fail; GUID not found)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.getItem(0)
      item.isDefined mustEqual false
    }

    "getItem (fail; wrong equipment requested)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(20, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer1, 1, 3)
      val item : Option[Equipment] = inv.getItem(beamer2)
      item.isDefined mustEqual false
    }

    "getItem (fail; no equipment requested)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.getItem(null)
      item.isDefined mustEqual false
    }

    "getItem (fail; no equipment at coordinates)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.getItem(4, 3)
      item.isDefined mustEqual false
    }

    "removeItem (guid)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val dropped : Option[Equipment] = inv.removeItem(50)
      inv.size mustEqual 0
      dropped.isDefined mustEqual true
      (dropped.get eq beamer) mustEqual true
    }

    "removeItem (equipment itself)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val dropped : Option[Equipment] = inv.removeItem(beamer)
      inv.size mustEqual 0
      dropped.isDefined mustEqual true
      (dropped.get eq beamer) mustEqual true
    }

    "removeItem (cartesian coordinates)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val dropped : Option[Equipment] = inv.removeItem(1, 3)
      inv.size mustEqual 0
      dropped.isDefined mustEqual true
      (dropped.get eq beamer) mustEqual true
    }

    "removeItem (fail; GUID not found)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      inv.removeItem(2) mustEqual None
      inv.size mustEqual 1
    }

    "removeItem (fail; wrong equipment requested)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(20, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer1, 1, 3)
      val item : Option[Equipment] = inv.removeItem(beamer2)
      item.isDefined mustEqual false
    }

    "removeItem (fail; no equipment requested)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.removeItem(null)
      item.isDefined mustEqual false
    }

    "removeItem (fail; no equipment at coordinates)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : ListInventory = ListInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.removeItem(4, 3)
      item.isDefined mustEqual false
    }

    "{object}.toString (empty)" in {
      val obj : ListInventory = ListInventory(12, 9)
      obj.toString mustEqual "{inventory(12x9): 0 items}"
    }

    "{object}.toString (1 item)" in {
      val beamer : Tool = Tool(50, 0)
      val obj : ListInventory = ListInventory(12, 9)
      obj.addItem(beamer, 1, 3)
      obj.toString mustEqual "{inventory(12x9): 1 items}"
    }

    "ListInventory.toString(object)" in {
      val obj : ListInventory = ListInventory(12, 9)
      ListInventory.toString(obj) mustEqual "{inventory(12x9): 0 items}"
    }
  }

  "GridInventory" should {
    "constructor" in {
      val inv : GridInventory = GridInventory(12, 9)
      inv.width mustEqual 12
      inv.height mustEqual 9
    }

    "get capacity" in {
      val inv : GridInventory = GridInventory(12, 9)
      inv.capacity mustEqual (12 * 9)
    }

    "get size" in {
      val inv : GridInventory = GridInventory(12, 9)
      inv.size mustEqual 0
    }

    "addItem (cartesian coordinates)" in {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, swapped : Option[Equipment]) = inv.addItem(beamer, 0,0)
      success mustEqual true
      swapped.isDefined mustEqual false
      inv.capacity mustEqual (12 * 9) //unchanged
      inv.size mustEqual 1
    }

    "addItem (hex coordinate)" in {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, swapped : Option[Equipment]) = inv.addItem(beamer, 0)
      success mustEqual true
      swapped.isDefined mustEqual false
      inv.capacity mustEqual (12 * 9) //unchanged
      inv.size mustEqual 1
    }

    "addItem (swap with overlapping item)" in {
      val beamer1 : Tool = Tool(0, 0)
      val beamer2 : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer1, 0, 0)
      inv.size mustEqual 1
      val (success : Boolean, swapped : Option[Equipment]) = inv.addItem(beamer2, 2,2) //only overlapping one square in the corner
      inv.size mustEqual 1 //same
      success mustEqual true
      swapped.isDefined mustEqual true
      (swapped.get eq beamer1) mustEqual true //beamer1 was removed
      inv.getItem(10).isDefined mustEqual true //found beamer2
    }

    "addItem (fail; single coordinate wrong)" in {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, -1)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; x-coordinate too low) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, 0, -1)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; x-coordinate too high) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, 0, 13)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; y-coordinate too low) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, -1, 0)
      success mustEqual false
      inv.size mustEqual 0
    }

    "addItem (fail; y-coordinate too high) " in  {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success : Boolean, _) = inv.addItem(beamer, 10, 0)
      success mustEqual false
      inv.size mustEqual 0
    }

    //all coordinates should check against the size of the inventory; only add tests for that for failing cases from now on

    "addItem (fail; inserting same item twice" in {
      val beamer : Tool = Tool(0, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success1 : Boolean, _) = inv.addItem(beamer, 0, 0)
      success1 mustEqual true
      val (success2 : Boolean, _) = inv.addItem(beamer, 3, 0)
      success2 mustEqual false
    }

    "addItem (fail; GUID already encountered?!)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      val (success1 : Boolean, _) = inv.addItem(beamer1, 0, 0)
      success1 mustEqual true
      val (success2 : Boolean, _) = inv.addItem(beamer2, 3, 0)
      success2 mustEqual false
    }

    "addItem (fail; no swap when two or more existing items overlap)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(20, 0)
      val beamer3 : Tool = Tool(30, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer1, 0, 0)
      inv.addItem(beamer2, 4, 0)
      inv.size mustEqual 2

      val (success : Boolean, value : Option[Equipment]) = inv.addItem(beamer3, 2, 2)
      inv.size mustEqual 2 //unchanged
      success mustEqual false
      value.isDefined mustEqual false
      inv.getItem(beamer3).isDefined mustEqual false

    }

    "getItem (guid)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val item : Option[Equipment] = inv.getItem(50)
      inv.size mustEqual 1
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true
    }

    "getItem (equipment itself)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val item : Option[Equipment] = inv.getItem(beamer)
      inv.size mustEqual 1
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true
    }

    "getItem (cartesian coordinates)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      var item : Option[Equipment] = None

      item = inv.getItem(1, 3) //upper left corner
      inv.size mustEqual 1
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true

      item = inv.getItem(2, 4) //middle
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true

      item = inv.getItem(3, 5) //lower right corner
      item.isDefined mustEqual true
      (item.get eq beamer) mustEqual true
    }

    "getItem (fail; GUID not found)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.getItem(0)
      item.isDefined mustEqual false
    }

    "getItem (fail; wrong equipment requested)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(20, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer1, 1, 3)
      val item : Option[Equipment] = inv.getItem(beamer2)
      item.isDefined mustEqual false
    }

    "getItem (fail; no equipment requested)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.getItem(null)
      item.isDefined mustEqual false
    }

    "getItem (fail; no equipment at coordinates)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.getItem(4, 3)
      item.isDefined mustEqual false
    }

    "removeItem (guid)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val dropped : Option[Equipment] = inv.removeItem(50)
      inv.size mustEqual 0
      dropped.isDefined mustEqual true
      (dropped.get eq beamer) mustEqual true
    }

    "removeItem (equipment itself)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val dropped : Option[Equipment] = inv.removeItem(beamer)
      inv.size mustEqual 0
      dropped.isDefined mustEqual true
      (dropped.get eq beamer) mustEqual true
    }

    "removeItem (cartesian coordinates)" in {
      val beamer : Tool = Tool(50, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      val dropped : Option[Equipment] = inv.removeItem(1, 3)
      inv.size mustEqual 0
      dropped.isDefined mustEqual true
      (dropped.get eq beamer) mustEqual true
    }

    "removeItem (fail; GUID not found)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      inv.size mustEqual 1
      inv.removeItem(2) mustEqual None
      inv.size mustEqual 1
    }

    "removeItem (fail; wrong equipment requested)" in {
      val beamer1 : Tool = Tool(10, 0)
      val beamer2 : Tool = Tool(20, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer1, 1, 3)
      val item : Option[Equipment] = inv.removeItem(beamer2)
      item.isDefined mustEqual false
    }

    "removeItem (fail; no equipment requested)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.removeItem(null)
      item.isDefined mustEqual false
    }

    "removeItem (fail; no equipment at coordinates)" in {
      val beamer : Tool = Tool(10, 0)
      val inv : GridInventory = GridInventory(12, 9)
      inv.addItem(beamer, 1, 3)
      val item : Option[Equipment] = inv.removeItem(4, 3)
      item.isDefined mustEqual false
    }

    "{object}.toString (empty)" in {
      val obj : GridInventory = GridInventory(12, 9)
      obj.toString mustEqual "{inventory(12x9): 0 items}"
    }

    "{object}.toString (1 item)" in {
      val beamer : Tool = Tool(50, 0)
      val obj : GridInventory = GridInventory(12, 9)
      obj.addItem(beamer, 1, 3)
      obj.toString mustEqual "{inventory(12x9): 1 items}"
    }

    "GridInventory.toString(object)" in {
      val obj : GridInventory = GridInventory(12, 9)
      GridInventory.toString(obj) mustEqual "{inventory(12x9): 0 items}"
    }
  }

  "Backpack" should {
    "constructor" in {
      val pack : Backpack = Backpack(10, 12, 9)
      pack.size mustEqual 0
      pack.capacity mustEqual 12*9
    }

    "owner" in {
      val pack : Backpack = Backpack(10, 12, 9)
      pack.owner mustEqual 10
    }

    "index" in {
      val pack : Backpack = Backpack(10, 12, 9)
      pack.offset mustEqual 134
    }

    "{object}.toString (empty)" in {
      val obj : Backpack = Backpack(10, 12, 9)
      obj.toString mustEqual "{backpack(12x9): 0 items}"
    }

    "{object}.toString (1 item)" in {
      val beamer = Tool(0, 0)
      val obj : Backpack = Backpack(10, 12, 9)
      obj.addItem(beamer, 0, 0)
      obj.toString mustEqual "{backpack(12x9): 1 items}"
    }

    "Backpack.toString(object)" in {
      val obj : Backpack = Backpack(10, 12, 9)
      Backpack.toString(obj) mustEqual "{backpack(12x9): 0 items}"
    }
  }
}
