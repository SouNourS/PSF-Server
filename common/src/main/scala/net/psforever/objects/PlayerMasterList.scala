// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import net.psforever.packet.game.{PlanetSideEmpire, PlanetSideGUID}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A class for storing very important mappings between PlayerAvatars, PlayerAvatar ids, and external identifiers for PlayerAvatars.
  */
final class PlayerMasterList {
  /**
    * When this instance was instantiated.
    */
  val started : Long = System.currentTimeMillis
  /**
    * This mapping coordinates the name of the PlayerAvatar to the PlayerAvatar instance itself.
    * The PlayerAvatar is built external to this global object and must be passed into it with the appropriate function call.
    * All PlayerAvatar instances are maintained by this mapping.
    */
  val nameToCharacter : mutable.HashMap[Int, PlayerAvatar] = mutable.HashMap[Int, PlayerAvatar]()
  /**
    * This mapping keeps track of an internal GUID and entrusts a certain name that GUID.
    * Whenever a name is required in context to something that happens, this external GUID would be contestually known.
    * This name follows from know that GUID via the mapping.</br>
    * The two-layered mechanism provides a means of control validation for the named PlayerAvatar.
    * If a request for this name is made from a session providing the wrong external id, it will be rejected.
    * Considerations have been made if this name is called in context to some other event (no validation).
    */
  val externToName : mutable.HashMap[Long, Int] = mutable.HashMap[Long, Int]()

  /**
    * Get the world population, broken down by PlayerAvatar faction.
    *
    * Exclude mods from these counts, if they are included in the same lists.
    * Include Black Ops as integrated into their normal factions.
    * @return a Tuple of three population values in order: NC, TR, VS
    */
  def getWorldPopulation : (Int, Int, Int) = {
    ( nameToCharacter.count(x => { x._2.faction == PlanetSideEmpire.NC}),
      nameToCharacter.count(x => { x._2.faction == PlanetSideEmpire.TR}),
      nameToCharacter.count(x => { x._2.faction == PlanetSideEmpire.VS}) )
  }

  /**
    * Get a PlayerAvatar representation.
    * @param guid the transformed GUID of the PlayerAvatar
    * @return an Option containing the PlayerAvatar object; None, if the GUID is unknown
    */
  def getPlayer(guid : PlanetSideGUID) : Option[PlayerAvatar] = {
    guid match {
      case PlanetSideGUID(id : Int) => getPlayer(id)
      case _ => None
    }
  }

  /**
    * Get a PlayerAvatar representation.
    * @param id the raw GUID of the PlayerAvatar
    * @return an Option containing the PlayerAvatar object; None, if the id is unknown
    */
  def getPlayer(id : Int) : Option[PlayerAvatar] = {
    nameToCharacter.get(id)
  }

  /**
    * Get a PlayerAvatar representation.
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @return an Option containing the PlayerAvatar object; None, if the external id is unknown
    */
  def getPlayer(externId : Long) : Option[PlayerAvatar] = {
    val nameForSession = externToName.get(externId)
    if(nameForSession.isDefined) {
      return nameToCharacter.get(nameForSession.get)
    }
    None
  }

  /**
    * Get a PlayerAvatar representation.
    * This is a "safe" get-method where the user who is controlling the character is required to be an origin point for the request.
    * This is the raw Int and Long overload.
    * @param id the raw GUID of the PlayerAvatar
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @return an Option containing the user's player character; None, the data is not related
    */
  def getPlayer(id : Int, externId : Long) : Option[PlayerAvatar] = {
    val nameForSession = externToName.get(externId)
    if(nameForSession.isDefined && nameForSession.get.equals(id))
      return nameToCharacter.get(id)
    None
    //TODO flag this session for funny business -- not asking for own character
  }

  /**
    * Mark that a PlayerAvatar with this id will be associated with this external id.
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @param id the raw GUID of the PlayerAvatar
    * @return the raw GUID of the PlayerAvatar being referenced by this external id; Int.MinValue, if the association fails
    */
  def userClaimsCharacter(externId : Long, id : Int) : Int = {
    externToName.filter(x => x._2 == id).foreach(x =>
      if(x._1 != externId)
        return Int.MinValue // name already reserved
    )
    externToName.filter(x => x._1 == externId).foreach(x =>
      if(x._2 != id)
        return Int.MinValue // sessionId already used
    )
    if(!nameToCharacter.contains(id))
      return Int.MinValue // sessionId already used

    externToName += (externId -> id)
    id
  }

  /**
    * Forget that a PlayerAvatar with this id will be associated with this external id.
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @return the raw GUID of the PlayerAvatar previously referenced by this external id; Int.MinValue, if the association fails
    */
  def userDissociatesCharacter(externId : Long) : Int = {
    val token = externToName.remove(externId)
    if(token.isDefined)
      return token.get
    Int.MinValue
  }

  /**
    * Forget that a PlayerAvatar with this id will be associated with this external id.
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @param id the raw GUID of the PlayerAvatar
    * @return the raw GUID of the PlayerAvatar previously referenced by this external id; Int.MinValue, if the association fails
    */
  def userDissociatesCharacter(externId : Long, id : Int) : Int = {
    val token = externToName.get(externId)
    if(token.isDefined && token.get == id) {
      externToName.remove(externId)
      return token.get
    }
    Int.MinValue
  }

  /**
    * These PlayerAvatars are known to the server but are not (currently) associated to any external ids.
    * @return a List of the unassociated ids
    */
  def getUnclaimedCharacters : List[Int] = {
    var unclaimed : ListBuffer[Int] = new ListBuffer[Int]
    if(externToName.size != nameToCharacter.size) {
      var claims: mutable.Map[Int, Long] = mutable.Map[Int, Long]()
      externToName.foreach { case (key: Long, value: Int) => claims += { value -> key } } // Reverse the sessionToName mapping for a quick lookup
      nameToCharacter.foreach { case (name, char) => if (!claims.contains(name)) unclaimed += name } // Collect unclaimed characters
    }
    unclaimed.toList
  }

  /**
    * Add a PlayerAvatar.
    * @param player the PlayerAvatar
    * @return true, if this PlayerAvatar was able to join (or re-join) the list; false, otherwise
    */
  def addPlayer(player : PlayerAvatar) : Boolean = {
    val guid : Int = player.guid
    if(nameToCharacter.contains(guid)) {
      // Character already on server; is it unowned?
      if(externToName.count(x => { x._2 == guid }) > 0) {
        //TODO flag for funny business -- another external id associated with this PlayerAvatar id
      }
      return false
    }

    nameToCharacter += (guid -> player)
    true
  }

  /**
    * Add a PlayerAvatar.
    * This is a "safe" attempt to add the player.
    * It should be called when an external id is prepared to be associated with the player.
    * @param player the PlayerAvatar
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @return true, if this PlayerAvatar was able to join (or re-join) the list; false, otherwise
    */
  def addPlayer(player : PlayerAvatar, externId : Long) : Boolean = {
    val guid : Int = player.guid
    if(externToName.contains(externId)) {
      if(externToName(externId) != guid) {
        //TODO flag for funny business -- prior claim to another PlayerAvatar
        return false
      }
    }
    if(nameToCharacter.contains(guid)) {
      // player already known; is it unassociated?
      if(externToName.count(x => { x._2 == guid && x._1 != externId }) > 0) {
        //TODO flag for funny business -- another external id associated with this PlayerAvatar id
        return false
      }
      return true
    }

    nameToCharacter += (guid -> player)
    externToName += (externId -> guid)
    true
  }

  /**
    * Remove a PlayerAvatar.
    * @param player the PlayerAvatar
    * @return always returns true
    */
  def removePlayer(player : PlayerAvatar) : Boolean = {
    removePlayer(player.guid)
  }

  /**
    * Remove a PlayerAvatar.
    * @param guid the transformed GUID of the PlayerAvatar
    * @return always returns true
    */
  def removePlayer(guid : PlanetSideGUID) : Boolean = {
    guid match {
      case PlanetSideGUID(id : Int) => removePlayer(id)
      case _ => false
    }
  }

  /**
    * Remove a PlayerAvatar.
    * Clean up any potential mappings connected to this player and its id, including its external id.
    * @param id the raw GUID of the PlayerAvatar
    * @return true, if the PlayerAvatar is removed; false, othwerwise
    */
  def removePlayer(id : Int) : Boolean = {
    if(!nameToCharacter.contains(id))
      return false

    nameToCharacter.remove(id)
    val session = externToName.filter(x => {
      x._2 == id
    })
    if(session.nonEmpty) {
      // Housecleaning
      session.foreach { case (key : Long, value : Int) => userDissociatesCharacter(key) }
    }
    true
  }

  /**
    * Remove a PlayerAvatar.
    * Clean up any potential mappings connected to this player and its id, including its external id.
    * @param externId an external id that should be associated with a PlayerAvatar id
    * @return true, if the PlayerAvatar associated with this external id is removable; false, otherwise
    */
  def removePlayer(externId : Long) : Boolean = {
    val token : Option[Int] = externToName.remove(externId)
    if(token.isDefined) {
      nameToCharacter.remove(token.get)
      return true
    }
    false
  }

  /**
    * Hastily remove all PlayerAvatars from the collection and clear all mappings and external id mappings.
    * @return an unsorted list of the PlayerAvatar ids that were still present
    */
  def shutdown : List[PlayerAvatar] = {
    val list = nameToCharacter.values.toList
    externToName.clear
    nameToCharacter.clear
    list
  }
}

/**
  * The player master list is a record that keeps track of all players and characters currently logged into the server, regardless of zone.
  * For the purpose of the explanation: a "player" is the end user human; a "character" is the digital representation on the server.<br>
  * The "player" is known by a session id that is important to the player's current connection to the server through their client.
  * The "character" is known by a character id (of some sort) that is persistent to the character.<br>
  * The master list is a really singleton.
  * <br>
  * Workflow:<br>
  * 1. Player selects a character from their client's login screen (CharacterRequestMessage, Selection).<br>
  * 2. An entry mapping the character id to the character.<br>
  * 3. During the character and world setup on the client, the player's session is mapped to the character id.<br>
  * 4. The player is logged into the server and has control over the character once the client finishes loading.<br>
  * 5. If the player times out of the server, the mapping between the session id and the character id is removed.<br>
  * 6. If the player rejoins the server soon enough, the mapping between a new session id and the character id is made.<br>
  * 7. If the player does not rejoin the server, eventually the mapping between the character and its id are removed.<br>
  * 8. If the player timesout his character gently by voluntarily leaving, the character mapping is removed switfly.<br>
  * 9. Neither the player nor the character are connected to the server anymore.<br>
  */
object PlayerMasterList {
  /**
    * An instance of the character and session records.
    * Under singleton design, there is ever only one.
    */
  private var instance : Option[PlayerMasterList] = None

  /**
    * Create a new instance of the underlying class to be returned each time, or return that already-created instance
    * @return the data structures
    */
  private def getInstance : PlayerMasterList = {
    if(instance.isEmpty)
      instance = Option(new PlayerMasterList)
    instance.get
  }

  /**
    * @see (class PlayerMasterList { ... }).getWorldPopulation
    */
  def getWorldPopulation : (Int, Int, Int) = {
    getInstance.getWorldPopulation
  }

  /**
    * @see (class PlayerMasterList { ... }).getPlayer(PlanetSideGUID)
    */
  def getPlayer(name : PlanetSideGUID) : Option[PlayerAvatar] = {
    getInstance.getPlayer(name)
  }

  /**
    * @see (class PlayerMasterList { ... }).getPlayer(Int)
    */
  def getPlayer(name : Int) : Option[PlayerAvatar] = {
    getInstance.nameToCharacter.get(name)
  }

  /**
    * @see (class PlayerMasterList { ... }).getPlayer(Long)
    */
  def getPlayer(externId : Long) : Option[PlayerAvatar] = {
    getInstance.getPlayer(externId)
  }

  /**
    * @see (class PlayerMasterList { ... }).getPlayer(Int, Long)
    */
  def getPlayer(name : Int, externId : Long) : Option[PlayerAvatar] = {
    getInstance.getPlayer(name, externId)
  }

  /**
    * @see (class PlayerMasterList { ... }).userClaimsCharacter(Long, Int)
    */
  def userClaimsCharacter(externId : Long, name : Int) : Int = {
    getInstance.userClaimsCharacter(externId, name)
  }

  /**
    * @see (class PlayerMasterList { ... }).userDissociatesCharacter(Long)
    */
  def userDissociatesCharacter(externId : Long) : Int = {
    getInstance.userDissociatesCharacter(externId)
  }

  /**
    * @see (class PlayerMasterList { ... }).userDissociatesCharacter(Long, Int)
    */
  def userDissociatesCharacter(externId : Long, name : Int) : Int = {
    getInstance.userDissociatesCharacter(externId, name)
  }

  /**
    * @see (class PlayerMasterList { ... }).getUnclaimedCharacters
    */
  def getUnclaimedCharacters : List[Int] = {
    getInstance.getUnclaimedCharacters
  }

  /**
    * @see (class PlayerMasterList { ... }).addPlayer(PlayerAvatar)
    */
  def addPlayer(player : PlayerAvatar) : Boolean = {
    getInstance.addPlayer(player)
  }

  /**
    * @see (class PlayerMasterList { ... }).addPlayer(PlayerAvatar, Long)
    */
  def addPlayer(player : PlayerAvatar, sessionId : Long) : Boolean = {
    getInstance.addPlayer(player, sessionId)
  }

  /**
    * @see (class PlayerMasterList { ... }).removePlayer(PlayerAvatar)
    */
  def removePlayer(player : PlayerAvatar) : Boolean = {
    getInstance.removePlayer(player.guid)
  }

  /**
    * @see (class PlayerMasterList { ... }).removePlayer(PlanetSideGUID)
    */
  def removePlayer(guid : PlanetSideGUID) : Boolean = {
    getInstance.removePlayer(guid)
  }

  /**
    * @see (class PlayerMasterList { ... }).removePlayer(Int)
    */
  def removePlayer(guid : Int) : Boolean = {
    getInstance.removePlayer(guid)
  }

  /**
    * @see (class PlayerMasterList { ... }).removePlayer(Long)
    */
  def removePlayer(externId : Long) : Boolean = {
    getInstance.removePlayer(externId)
  }

  /**
    * @see (class PlayerMasterList { ... }).shutdown
    */
  def shutdown : List[PlayerAvatar] = {
    val records : List[PlayerAvatar] = getInstance.shutdown
    instance = null
    records
  }
}
