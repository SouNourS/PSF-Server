// Copyright (c) 2016 PSForever.net to present
import net.psforever.objects._
import net.psforever.packet.game.{PlanetSideEmpire, PlanetSideGUID}
import org.specs2.mutable._

class PlayerAvatarTest extends Specification {
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
      player.setExoSuitType(0)
      player.getEquipmentInHolster(0).isDefined mustEqual false

      val (success, _) = player.setEquipmentInHolster(0, beamer)
      success mustEqual true
      player.getEquipmentInHolster(0).isDefined mustEqual true
      (player.getEquipmentInHolster(0).get eq beamer) mustEqual true
    }

    "setEquipmentInHolster (swapped equipment)" in {
      val player : PlayerAvatar = PlayerAvatar(0)
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
      player.setExoSuitType(0)
      player.setEquipmentInHolster(0, beamer)
      player.getHolster(0).getEquipment.isDefined mustEqual true
      (player.getHolster(0).getEquipment.get eq beamer) mustEqual true

      val amp : Tool = Tool(2, EquipmentSize.PISTOL, 3)
      val (success, discard) = player.setEquipmentInHolster(0, amp)
      success mustEqual true
      player.getEquipmentInHolster(0).isDefined mustEqual true
      (player.getEquipmentInHolster(0).get eq amp) mustEqual true
      discard.isDefined mustEqual true
      (discard.get eq beamer) mustEqual true
    }

    "setEquipmentInHolster (dropped equipment)" in {
      val player : PlayerAvatar = PlayerAvatar(0)
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
      player.setExoSuitType(0)
      player.setEquipmentInHolster(0, beamer)

      player.setUsedHolster(0)
      player.setUsedHolster(255)
      player.getUsedHolster mustEqual 255
    }

    "setUsedHolster (dropped equipment, put away empty holster)" in {
      val player : PlayerAvatar = PlayerAvatar(0)
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
      val beamer2 : Tool = Tool(2, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
      val beamer : Tool = Tool(1, EquipmentSize.PISTOL, 0)
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
}
