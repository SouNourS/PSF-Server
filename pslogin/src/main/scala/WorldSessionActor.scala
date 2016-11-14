// Copyright (c) 2016 PSForever.net to present
import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Cancellable, MDCContextAware}
import net.psforever.packet.{PlanetSideGamePacket, _}
import net.psforever.packet.control._
import net.psforever.packet.game._
import scodec.Attempt.{Failure, Successful}
import scodec.bits._
import org.log4s.MDC
import MDCContextAware.Implicits._
import net.psforever.types.ChatMessageType
import scodec.codecs._

import scala.collection.mutable
import scala.collection.immutable

class WorldSessionActor extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger

  private case class PokeClient()

  var sessionId : Long = 0
  var leftRef : ActorRef = ActorRef.noSender
  var rightRef : ActorRef = ActorRef.noSender

  var clientKeepAlive : Cancellable = null

  override def postStop() = {
    if(clientKeepAlive != null)
      clientKeepAlive.cancel()
  }

  def receive = Initializing

  def Initializing : Receive = {
    case HelloFriend(sessionId, right) =>
      this.sessionId = sessionId
      leftRef = sender()
      rightRef = right.asInstanceOf[ActorRef]

      context.become(Started)
    case _ =>
      log.error("Unknown message")
      context.stop(self)
  }

  def Started : Receive = {
    case ctrl @ ControlPacket(_, _) =>
      handlePktContainer(ctrl)
    case game @ GamePacket(_, _, _) =>
      handlePktContainer(game)
      // temporary hack to keep the client from disconnecting
    case PokeClient() =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))
    case default => failWithError(s"Invalid packet class received: $default")
  }

  def handlePkt(pkt : PlanetSidePacket) : Unit = pkt match {
    case ctrl : PlanetSideControlPacket =>
      handleControlPkt(ctrl)
    case game : PlanetSideGamePacket =>
      handleGamePkt(game)
    case default => failWithError(s"Invalid packet class received: $default")
  }

  def handlePktContainer(pkt : PlanetSidePacketContainer) : Unit = pkt match {
    case ctrl @ ControlPacket(opcode, ctrlPkt) =>
      handleControlPkt(ctrlPkt)
    case game @ GamePacket(opcode, seq, gamePkt) =>
      handleGamePkt(gamePkt)
    case default => failWithError(s"Invalid packet container class received: $default")
  }

  def handleControlPkt(pkt : PlanetSideControlPacket) = {
    pkt match {
      case SlottedMetaPacket(slot, subslot, innerPacket) =>
        sendResponse(PacketCoding.CreateControlPacket(SlottedMetaAck(slot, subslot)))

        PacketCoding.DecodePacket(innerPacket) match {
          case Failure(e) =>
            log.error(s"Failed to decode inner packet of SlottedMetaPacket: $e")
          case Successful(v) =>
            handlePkt(v)
        }
      case sync @ ControlSync(diff, unk, f1, f2, f3, f4, fa, fb) =>
        log.debug(s"SYNC: ${sync}")
        val serverTick = Math.abs(System.nanoTime().toInt) // limit the size to prevent encoding error
        sendResponse(PacketCoding.CreateControlPacket(ControlSyncResp(diff, serverTick,
          fa, fb, fb, fa)))
      case MultiPacket(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(s"Failed to decode inner packet of MultiPacket: $e")
            case Successful(v) =>
              handlePkt(v)
          }
        }
      case MultiPacketEx(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(s"Failed to decode inner packet of MultiPacketEx: $e")
            case Successful(v) =>
              handlePkt(v)
          }
        }
      case default =>
        log.debug(s"Unhandled ControlPacket $default")
    }
  }

  // XXX: hard coded ObjectCreateMessage
  val objectHex = hex"18 57 0C 00 00 BC 84 B0  06 C2 D7 65 53 5C A1 60 00 01 34 40 00 09 70 49  00 6C 00 6C 00 6C 00 49 00 49 00 49 00 6C 00 6C  00 6C 00 49 00 6C 00 49 00 6C 00 6C 00 49 00 6C  00 6C 00 6C 00 49 00 6C 00 6C 00 49 00 84 52 70  76 1E 80 80 00 00 00 00 00 3F FF C0 00 00 00 20  00 00 0F F6 A7 03 FF FF FF FF FF FF FF FF FF FF  FF FF FF FF FF FD 90 00 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00  00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46  86 C7 00 00 00 80 00 00 12 40 78 70 65 5F 73 61  6E 63 74 75 61 72 79 5F 68 65 6C 70 90 78 70 65  5F 74 68 5F 66 69 72 65 6D 6F 64 65 73 8B 75 73  65 64 5F 62 65 61 6D 65 72 85 6D 61 70 31 33 00  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02  60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80  00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41  C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00  00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05  48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32  00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90  00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8  05 26 40 00 03 20 06 C2 58 00 A7 88 00 00 02 00  00 80 00 00 "
  val traveler = Traveler(this, "home3")

  def handleGamePkt(pkt : PlanetSideGamePacket) = pkt match {
    case ConnectToWorldRequestMessage(server, token, majorVersion, minorVersion, revision, buildDate, unk) =>

      val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"

      log.info(s"New world login to ${server} with Token:${token}. ${clientVersion}")

      // ObjectCreateMessage
      sendRawResponse(objectHex)
      // XXX: hard coded message
      sendRawResponse(hex"14 0F 00 00 00 10 27 00  00 C1 D8 7A 02 4B 00 26 5C B0 80 00 ")

      // NOTE: PlanetSideZoneID just chooses the background
      sendResponse(PacketCoding.CreateGamePacket(0,
        CharacterInfoMessage(PlanetSideZoneID(1), 0, PlanetSideGUID(0), true, 0)))
    case msg @ CharacterRequestMessage(charId, action) =>
      log.info("Handling " + msg)

      action match {
        case CharacterRequestAction.Delete =>
          sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(false, Some(1))))
        case CharacterRequestAction.Select =>
          PacketCoding.DecodeGamePacket(objectHex).require match {
            case obj @ ObjectCreateMessage(len, cls, guid, _, _) =>
              log.debug("Object: " + obj)
              // LoadMapMessage 13714 in mossy .gcap
              // XXX: hardcoded shit
              sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))
              val home3 = Zone.get("home3").get
              Transfer.loadMap(traveler, home3)
              Transfer.loadSelf(traveler, home3.default)

              // These object_guids are specfic to VS Sanc
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.VS))) //HART building C
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.NC))) //South Villa Gun Tower

              sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(0, 4653056, 0, 0, 32, 65)))
              sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
              sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), 32))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate

              sendResponse(PacketCoding.CreateGamePacket(0,BuildingInfoUpdateMessage(
                PlanetSideGUID(6),   //Ceryshen
                PlanetSideGUID(2),   //Anguta
                8,                   //80% NTU
                true,                //Base hacked
                PlanetSideEmpire.NC, //Base hacked by NC
                600000,              //10 minutes remaining for hack
                PlanetSideEmpire.VS, //Base owned by VS
                0,                   //!! Field != 0 will cause malformed packet. See class def.
                PlanetSideGeneratorState.Critical, //Generator critical
                true,                //Respawn tubes destroyed
                true,                //Force dome active
                16,                  //Tech plant lattice benefit
                0,
                0,                   //!! Field > 0 will cause malformed packet. See class def.
                0,
                false,
                8,                   //!! Field != 8 will cause malformed packet. See class def.
                true,                //Boosted spawn room pain field
                true)))              //Boosted generator room pain field

              import scala.concurrent.duration._
              import scala.concurrent.ExecutionContext.Implicits.global
              clientKeepAlive = context.system.scheduler.schedule(0 seconds, 500 milliseconds, self, PokeClient())
          }
        case default =>
          log.error("Unsupported " + default + " in " + msg)
      }
    case msg @ CharacterCreateRequestMessage(name, head, voice, gender, empire) =>
      log.info("Handling " + msg)

      sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(true, None)))
      sendResponse(PacketCoding.CreateGamePacket(0,
        CharacterInfoMessage(PlanetSideZoneID(0), 0, PlanetSideGUID(0), true, 0)))

    case KeepAliveMessage(code) =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))

    case msg @ PlayerStateMessageUpstream(avatar_guid, pos, vel, unk1, aim_pitch, unk2, seq_time, unk3, is_crouching, unk4, unk5, unk6, unk7, unk8) =>
      //log.info("PlayerState: " + msg)

    case msg @ ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
        log.info("Chat: " + msg)
      }

      CSRZone.read(traveler, msg)
      CSRWarp.read(traveler, msg)

      // TODO: handle this appropriately
      if(messagetype == ChatMessageType.CMT_QUIT) {
        sendResponse(DropCryptoSession())
        sendResponse(DropSession(sessionId, "user quit"))
      }

      // TODO: Depending on messagetype, may need to prepend sender's name to contents with proper spacing
      // TODO: Just replays the packet straight back to sender; actually needs to be routed to recipients!
      sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))

    case msg @ ChangeFireModeMessage(item_guid, fire_mode) =>
      log.info("ChangeFireMode: " + msg)

    case msg @ ChangeFireStateMessage_Start(item_guid) =>
      log.info("ChangeFireState_Start: " + msg)

    case msg @ ChangeFireStateMessage_Stop(item_guid) =>
      log.info("ChangeFireState_Stop: " + msg)

    case msg @ EmoteMsg(avatar_guid, emote) =>
      log.info("Emote: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, EmoteMsg(avatar_guid, emote)))

    case msg @ DropItemMessage(item_guid) =>
      log.info("DropItem: " + msg)

    case msg @ ReloadMessage(item_guid, ammo_clip, unk1) =>
      log.info("Reload: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ReloadMessage(item_guid, 123, unk1)))

    case msg @ ObjectHeldMessage(avatar_guid, held_holsters, unk1) =>
      log.info("ObjectHeld: " + msg)

    case msg @ AvatarJumpMessage(state) =>
      //log.info("AvatarJump: " + msg)

    case msg @ RequestDestroyMessage(object_guid) =>
      log.info("RequestDestroy: " + msg)
      // TODO: Make sure this is the correct response in all cases
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg @ ObjectDeleteMessage(object_guid, unk1) =>
      log.info("ObjectDelete: " + msg)

    case msg @ MoveItemMessage(item_guid, avatar_guid_1, avatar_guid_2, dest, unk1) =>
      log.info("MoveItem: " + msg)

    case msg @ ChangeAmmoMessage(item_guid, unk1) =>
      log.info("ChangeAmmo: " + msg)

    case msg @ UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9) =>
      log.info("UseItem: " + msg)
      // TODO: Not all fields in the response are identical to source in real packet logs (but seems to be ok)
      // TODO: Not all incoming UseItemMessage's respond with another UseItemMessage (i.e. doors only send out GenericObjectStateMsg)
      sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9)))
      // TODO: This should only actually be sent to doors upon opening; may break non-door items upon use
      sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(object_guid, 16)))

    case msg @ GenericObjectStateMsg(object_guid, unk1) =>
      log.info("GenericObjectState: " + msg)

    case msg @ ItemTransactionMessage(terminal_guid, transaction_type, item_page, item_name, unk1, item_guid) =>
      log.info("ItemTransaction: " + msg)

    case msg @ WeaponDelayFireMessage(seq_time, weapon_guid) =>
      log.info("WeaponDelayFire: " + msg)

    case msg @ WeaponFireMessage(seq_time, weapon_guid, projectile_guid, shot_origin, unk1, unk2, unk3, unk4, unk5, unk6, unk7) =>
      log.info("WeaponFire: " + msg)

    case msg @ HitMessage(seq_time, projectile_guid, unk1, hit_info, unk2, unk3, unk4) =>
      log.info("Hit: " + msg)

    case msg @ AvatarFirstTimeEventMessage(avatar_guid, object_guid, unk1, event_name) =>
      log.info("AvatarFirstTimeEvent: " + msg)

    case default => log.debug(s"Unhandled GamePacket ${pkt}")
  }

  def failWithError(error : String) = {
    log.error(error)
    //sendResponse(PacketCoding.CreateControlPacket(ConnectionClose()))
  }

  def sendResponse(cont : PlanetSidePacketContainer) : Unit = {
    log.trace("WORLD SEND: " + cont)
    sendResponse(cont.asInstanceOf[Any])
  }

  def sendResponse(msg : Any) : Unit = {
    MDC("sessionId") = sessionId.toString
    rightRef !> msg
  }

  def sendRawResponse(pkt : ByteVector) = {
    log.trace("WORLD SEND RAW: " + pkt)
    sendResponse(RawPacket(pkt))
  }

  def sendToSelf(msg : PlanetSidePacketContainer) : Unit = {
    sendResponse(msg)
  }

  def sendToSelf(msg : ByteVector) : Unit = {
    sendRawResponse(msg)
  }
}

// for development and fun; please remove this once we have continental transfer working
// at least, move it to a more presentable folder once we refactor WorldSessionActor
class Traveler(private val session : WorldSessionActor) {
  val player : ByteVector = session.objectHex
  var zone : String = ""

  def sendToSelf(msg : ByteVector) : Unit = {
    this.session.sendRawResponse(msg)
  }

  def sendToSelf(msg : PlanetSidePacketContainer) : Unit = {
    this.session.sendResponse(msg)
  }
}

object Traveler {
  def apply(session : WorldSessionActor) : Traveler = new Traveler(session)

  def apply(session : WorldSessionActor, zoneId : String) : Traveler = {
    val traveler = new Traveler(session)
    traveler.zone = zoneId
    traveler
  }
}

object CSRZone {
  def read(traveler : Traveler, msg : ChatMsg) : Boolean = {
    if(!isProperRequest(msg))
      return false
    val buffer = decomposeMessage(msg.contents)
    if(buffer.length == 0 || buffer(0).equals("-help")) {
      help(traveler)
      return false
    }

    var zoneId = ""
    for(o <- buffer) {
      if(o.equals("-list")) {
        if(zoneId.equals("")) {
          CSRZone.list(traveler)
          return false
        }
      }
      else if(zoneId.equals(""))
        zoneId = o
    }

    val zoneOpt = Zone.get(zoneId)
    if(zoneOpt.isEmpty) {
      CSRZone.error(traveler)
      return false
    }
    traveler.zone = zoneId
    val zone = zoneOpt.get
    Transfer.transfer(traveler, zone, zone.default)
    true
  }

  def isProperRequest(msg : ChatMsg) : Boolean ={
    msg.messageType == ChatMessageType.CMT_ZONE
  }

  private def decomposeMessage(msg : String) : Array[String] = {
    msg.trim.toLowerCase.split("//s*")
  }

  def help(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "use: /zone [<id>] | [-list]", None))
    )
  }

  def list(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", Zone.list, None))
    )
  }

  def error(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "error: /zone must be given a valid zonename (use '/zone -list')", None))
    )
  }
}

object CSRWarp {
  def read(traveler : Traveler, msg : ChatMsg) : Boolean = {
    if(!isProperRequest(msg))
      return false
    val buffer = decomposeMessage(msg.contents)
    if(buffer.length == 0 || buffer(0).equals("-help")) {
      help(traveler)
      return false
    }

    var destId = ""
    for(o <- buffer) {
      if(o.equals("-list")) {
        if(destId.equals("")) {
          CSRWarp.list(traveler)
          return false
        }
      }
      else if(destId.equals(""))
        destId = o
    }

    val zone = Zone.get(traveler.zone).get
    if(zone.locations.get(destId).isEmpty) {
      CSRWarp.error(traveler)
      return false
    }
    Transfer.transfer(traveler, zone, zone.locations(destId))
    true
  }

  def isProperRequest(msg : ChatMsg) : Boolean ={
    msg.messageType == ChatMessageType.CMT_WARP
  }

  private def decomposeMessage(msg : String) : Array[String] = {
    msg.trim.toLowerCase.split("//s*")
  }

  def help(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "use: /warp [<id> | <x> <y> <z>] | [-list]", None))
    )
  }

  def list(traveler : Traveler) : Unit = {
    val out = Zone.listLocations(Zone.get(traveler.zone).get)
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", out, None))
    )
  }

  def error(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "error: /warp must be given a valid location in this zone", None))
    )
  }
}

object Transfer {
  def transfer(traveler : Traveler, zone : Zone, destination : (Int, Int, Int)) : Unit = {
    disposeSelf(traveler)
    loadMap(traveler, zone)
    loadSelf(traveler, destination)
  }

  //I'm actually uncertain if we really need to do this.  Better safe than sorry for now.
  private def disposeSelf(traveler : Traveler) : Unit = {
    //dispose inventory
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(76),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(78),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(80),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(83),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(84),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(85),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(86),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(87),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(88),4)))
    //dispose self
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(75),4)))
  }

  def loadMap(traveler : Traveler, zone : Zone) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, LoadMapMessage(zone.map, zone.zonename, 40100,25,true,3770441820L)))
  }

  def loadSelf(traveler : Traveler, loc : (Int, Int, Int)) : Unit = {
    //calculate bit representation of modified coordinates
    val x : BitVector = uintL(12).encode(loc._1/2).toOption.get
    val y : BitVector = uintL(12).encode(loc._2/2).toOption.get
    val z : BitVector = uintL(12).encode(loc._3/4).toOption.get
    //dissect ObjectCreateMessage data portion
    val firstPart : BitVector = traveler.player.toBitVector.take(76) //first part
    var temp : BitVector = traveler.player.toBitVector.drop(88) //first part + 'x' were removed
    val secondPart : BitVector = temp.take(8) //first spacer
    temp = temp.drop(20) //first spacer and 'y' were removed
    val thirdPart : BitVector = temp.take(8) //second spacer
    //reconstitute ObjectCreateMessage data around new coordinates
    temp = firstPart ++ x ++ secondPart ++ y ++ thirdPart ++ z ++ temp.drop(20) //second spacer and 'z' were removed
    //send
    traveler.sendToSelf(temp.toByteVector)
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(PlanetSideGUID(75),0,0)))
  }
}

class Zone(val alias : String, val map : String, val zonename : String) {
  val locations : mutable.HashMap[String, (Int, Int, Int)] = mutable.HashMap()
  var default = (0,0,0)
}

object Zone {
  def apply(name : String, map : String, title : String) : Zone = new Zone(name, map, title)

  def get(zoneId : String) : Option[Zone] = {
    var zId = zoneId.toLowerCase
    if(alias.get(zId).isDefined)
      zId = alias(zId)

    zones.get(zId)
  }

  def list : String = {
    "zonenames: z1 - z10, home1 - home3, tzshtr, tzdrtr, c1 - c6, i1 - 14; zones are also aliased their continent name"
  }

  def listLocations(zone : Zone) : String = {
    var out : String = "shortcuts: "
    if(zone.locations.isEmpty)
      out += "none"
    else {
      for(name <- zone.locations.keysIterator) {
        out += name + ", "
      }
    }
    out
  }

  //the /zone command should use these names
  private val zones = immutable.HashMap[String, Zone](
    "z1" -> Zone("Solsar", "map01", "z1"),
    "z1" -> Zone("Hossin", "map02", "z2"),
    "z3" -> Zone("Cyssor", "map03", "z3"),
    "z4" -> Zone("Ishundar", "map04", "z4"),
    "z5" -> Zone("Forseral", "map05", "z5"),
    "z6" -> Zone("Ceryshen", "map06", "z6"),
    "z7" -> Zone("Esamir","map07", "z7"),
    "z8" -> Zone("Oshur", "map08", "z8"),
    "z9" -> Zone("Searhus","map09", "z9"),
    "z10" -> Zone("Amerish","map10", "z10"),
    "home1" -> Zone("NC Sanctuary", "map11", "home1"),
    "home2" -> Zone("TR Sanctuary", "map12", "home2"),
    "home3" -> Zone("VS Sanctuary", "map13", "home3"),
    "tzshtr" -> Zone("VR Shooting Range", "map14", "tzshtr"),
    "tzdrtr" -> Zone("VR Driving Range","map15", "tzdrtr"),
    "c1" -> Zone("Supai", "ugd01", "c1"),
    "c2" -> Zone("Hunhau", "ugd02", "c2"),
    "c3" -> Zone("Adlivun", "ugd03", "c3"),
    "c4" -> Zone("Byblos", "ugd04", "c4"),
    "c5" -> Zone("Annwn", "ugd05", "c5"),
    "c6" -> Zone("Drugaskan", "ugd06", "c6"),
    "i4" -> Zone("Nexus", "map96", "i4"),
    "i3" -> Zone("Desolation", "map97", "i3"),
    "i2" -> Zone("Ascension", "map98", "i2"),
    "i1" -> Zone("Extinction", "map99", "i1")
  )

  //for the benefit of utility, these names can be used too (for now)
  private val alias = immutable.HashMap[String, String](
    "solsar" -> "z1",
    "hossin" -> "z2",
    "cyssor" -> "z3",
    "ishundar" -> "z4",
    "forseral" -> "z5",
    "ceryshen" -> "z6",
    "esamir" -> "z7",
    "oshur" -> "z8",
    "searhus" -> "z9",
    "amerish" -> "z10",
    "nc-sanctuary" -> "home1",
    "tr-sanctuary" -> "home2",
    "vs-sanctuary" -> "home3",
    "shooting" -> "tzshtr",
    "driving" -> "tzdrtr",
    "supai" -> "c1",
    "hunhau" -> "c2",
    "adlivun" -> "c3",
    "byblos" -> "c4",
    "annwn" -> "c5",
    "drugaskan" -> "c6",
    "nexus" -> "i4",
    "desolation" -> "i3",
    "ascension" -> "i2",
    "extinction" -> "i1"
  )
  zones("home3").default = (3675, 2727, 91)
}
