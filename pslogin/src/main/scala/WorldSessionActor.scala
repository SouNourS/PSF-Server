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
//val objectHex = hex"18 570C0000 BC 8 4B00 6 C 2D7 65 535 C A 160 00 01 34 40 00 0970 49006C006C006C004900490049006C006C006C0049006C0049006C006C0049006C006C006C0049006C006C004900 84 52 70 76 1E 80 80 00 00 00 00 00 3F FF C0 00 00 00 20 00 00 0F F6 A7 03 FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFD 90 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00 00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46 86 C7 00 00 00 80 00 00 12 40 78 70 65 5F 73 61 6E 63 74 75 61 72 79 5F 68 65 6C 70 90 78 70 65 5F 74 68 5F 66 69 72 65 6D 6F 64 65 73 8B 75 73 65 64 5F 62 65 61 6D 65 72 85 6D 61 70 31 33 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02 60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80 00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41 C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00 00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05 48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32 00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90 00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8 05 26 40 00 03 20 06 C2 58 00 A7 88 00 00 02 00 00 80 00 00"
//                                          0 6 xxx B3 yyy 6 F zzz 00 0a a2                                                                                                                                                                                                                        00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00 00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46 86 C7 00 00 00 00 00 00
  val objectHex = hex"18 570C0000 BC 8 4B00 0 6 F00 B3 F01 6 F 150 40 02 03 40 00 0970 49006C006C006C004900490049006C006C006C0049006C0049006C006C0049006C006C006C0049006C006C004900 04 00 70 76 1E 80 80 00 00 00 00 00 3F FF C0 00 00 00 20 00 00 0F F6 A7 03 C8000003 08000003 FFFFFFFE 94040002 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00 00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46 86 C7 00 00 00 80 00 00 12 40 7870655F73616E6374756172795F68656C70 90 7870655F74685F666972656D6F646573 8B 757365645F6265616D6572 85 6D61703133 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02 60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80 00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41 C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00 00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05 48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32 00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90 00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8 05 26 40 00 03 20 06 C2 59 00 A7 88 00 00 02 00 00 80 00 00"

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
              sendRawResponse(hex"D5 0B 00 00 00 01 0A E4 0C 02 48 70 75 72 63 68 61 73 65 5F 65 78 65 6D 70 74 5F 76 73 80 92 70 75 72 63 68 61 73 65 5F 65 78 65 6D 70 74 5F 74 72 80 92 70 75 72 63 68 61 73 65 5F 65 78 65 6D 70 74 5F 6E 63 80 11 00 01 14 A4 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 12 00 01 14 A4 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 13 00 01 14 A4 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 14 00 01 14 A4 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 15 00 01 14 A4 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 16 00 01 14 A4 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 1D 00 15 0A 60 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 54 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 76 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 87 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C7 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C8 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 26 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 52 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 AD 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B0 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CE 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 D6 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 2C 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 82 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 83 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CA 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 61 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 9B 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 DA 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 1E 00 15 0A 60 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 54 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 76 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 87 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C7 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C8 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 26 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 52 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 AD 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B0 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CE 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 D6 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 2C 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 82 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 83 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CA 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 61 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 9B 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 DA 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 1F 00 15 0A 60 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 54 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 76 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 87 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C7 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C8 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 26 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 52 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 AD 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B0 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CE 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 D6 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 2C 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 82 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 83 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CA 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 61 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 9B 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 DA 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 20 00 15 0A 60 04 02 1C 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 54 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 76 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 87 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C7 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 C8 00 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 26 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 52 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 AD 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B0 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CE 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 D6 20 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 2C 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 82 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 83 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 B9 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 CA 40 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 61 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 9B 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65 DA 60 20 10 E0 61 6C 6C 6F 77 65 64 85 66 61 6C 73 65")
              sendRawResponse(hex"2C 00 00 70 01 00 00 00")
              sendRawResponse(hex"E6 B8 01 06 01 00 8B 46 00 72 00 61 00 67 00 4C 00 41 00 4E 00 64 00 49 00 4E 00 43 00 84 46 00 72 00 61 00 67 00 0A 00 00 00 0A FF")

              // LoadMapMessage 13714 in mossy .gcap
              // XXX: hardcoded shit
              sendResponse(PacketCoding.CreateGamePacket(0, LoadMapMessage("ugd01","c1",40100,25,true,3770441820L))) //VS Sanctuary
              sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))
              sendRawResponse(objectHex)

              // These object_guids are specfic to VS Sanc
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.VS))) //HART building C
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.NC))) //South Villa Gun Tower

              sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(0, 4653056, 0, 0, 32, 65)))
              sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
              sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), 32))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate

//              sendResponse(PacketCoding.CreateGamePacket(0,BuildingInfoUpdateMessage(
//                PlanetSideGUID(6),   //Ceryshen
//                PlanetSideGUID(2),   //Anguta
//                8,                   //80% NTU
//                true,                //Base hacked
//                PlanetSideEmpire.NC, //Base hacked by NC
//                600000,              //10 minutes remaining for hack
//                PlanetSideEmpire.VS, //Base owned by VS
//                0,                   //!! Field != 0 will cause malformed packet. See class def.
//                PlanetSideGeneratorState.Critical, //Generator critical
//                true,                //Respawn tubes destroyed
//                true,                //Force dome active
//                16,                  //Tech plant lattice benefit
//                0,
//                0,                   //!! Field > 0 will cause malformed packet. See class def.
//                0,
//                false,
//                8,                   //!! Field != 8 will cause malformed packet. See class def.
//                true,                //Boosted spawn room pain field
//                true)))              //Boosted generator room pain field
//             sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(23), PlanetSideGUID(10175),
//                10,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(8),
//                9,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(7),
//                8,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(28), PlanetSideGUID(10257),
//                7,false,PlanetSideEmpire.VS,0,PlanetSideEmpire.VS,0,PlanetSideGeneratorState.Normal,false,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(28), PlanetSideGUID(10259),
//                6,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,false,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(28), PlanetSideGUID(10304),
//                5,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,false,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(4),
//                4,false,PlanetSideEmpire.VS,0,PlanetSideEmpire.VS,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(5),
//                3,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(6),
//                2,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(1),
//                1,false,PlanetSideEmpire.VS,0,PlanetSideEmpire.VS,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(2),
//                1,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(3),
//                1,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))

              sendRawResponse(hex"BE 88 4D B5 6C 46 AE D0 A0 04 00")
              sendRawResponse(hex"08 4B 00 84 DB 56 C4 6A ED 0A 00 00 00 00 10 1B 83 04 1F F6 8A BE 5D 57 A2 00 00 00 00 AF 00 0F 00 1B 82 04 1F FB CA BF 96 AF 02 00 00 00 00 AF 00 0F 00 1B 81 04 1F E6 F4 BF 32 F7 22 00 00 00 00 AF 00 0F 00")
              sendResponse(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(PlanetSideGUID(guid),0,0)))
              sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_EXPANSIONS,true,"","1 on",None)))

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
      if(false && is_crouching && !ArmorChangedMessage.changeOnce) {
        ArmorChangedMessage.changeOnce = true
        //carefully delete inventory
        sendRawResponse(hex"19 4C00 00") //suppressor/beamer?
        sendRawResponse(hex"19 4E00 00") //beamer/suppressor?
        sendRawResponse(hex"19 5300 00") //ammo
        sendRawResponse(hex"19 5400 00") //ammo
        sendRawResponse(hex"19 5500 00") //ammo
        sendRawResponse(hex"19 5600 00") //ammo
        sendRawResponse(hex"19 5700 00") //ammo
        sendRawResponse(hex"19 5800 00") //ammo
//        sendResponse(PacketCoding.CreateGamePacket(0, ArmorChangedMessage(avatar_guid, 0, 0)))
//        //see capture "last", starting @ line 688
//        //note: adding a medkit creates the shortcut if it doesn't exist and dispatches an 0x28 packet to the server
//        //sendRawResponse(hex"18 7C000000 2580 692 5C0F 9E C0000018000") //reaver fury rockets, 2,6
//        sendRawResponse(hex"18 7C000000 2580 79A 0D06 86 C8000020000") //buckshot, 0,0
//        sendRawResponse(hex"18 7C000000 2580 0E0 0005 A1 C8000064000") //9mm, 3,0
//        sendRawResponse(hex"18 7C000000 2580 0E0 1506 BC C8000064000") //9mm, 6,0
//        sendRawResponse(hex"18 7C000000 2580 0C2 F805 A6 C8000002000") //medkit, 3,5
//        sendRawResponse(hex"18 7C000000 2580 0C2 F604 B8 C8000002000") //medkit, 5,5
//        //sendRawResponse(hex"18 87000000 2580 100 690B 80 8800000200008") // ACE, Boomer, pistol slot 1
//        sendRawResponse(hex"18 7C000000 2580 0C2 1A06 CA C8000002000") //medkit, 7,5
//        sendRawResponse(hex"18 DC000000 2580 542 4407 80 480000020000C04A941A0B019000000C000") // plasma grenades, pistol slot 1
//        sendRawResponse(hex"18 97000000 2580 6C2 9F05 81 48000002000080000") //rek, pistol slot 2
//        sendRawResponse(hex"18 DC000000 2580 501 6A07 B6 480000020000C04A137A0B019000000C000") // jammer grenades, 5,3
//        sendRawResponse(hex"18 DC000000 2580 501 4406 C8 480000020000C04A13C209019000000C000") // jammer grenades, 7,3
//        sendRawResponse(hex"18 DC000000 2580 2C9 B905 82 480000020000C041C00C0B0190000078000") // gauss, rifle slot 1
//        sendRawResponse(hex"18 DC000000 2580 181 F804 89 480000020000C04F35AE0D0190000030000") // sweeper, 0,3
      }

    case msg @ ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
        log.info("Chat: " + msg)
      }

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

    case msg @ AvatarGrenadeStateMessage(player_guid, state) =>
      log.info("AvatarGrenadeStateMessage: " + msg)

    case msg @ SplashHitMessage(bytes) =>
      log.info("SplashHitMessage: " + bytes.toString)

    case msg @ ZipLineMessage(player_guid, unk1, unk2, unk3, unk4, unk5, unk6) =>
      log.info("ZipLineMessage: " + msg)

    case msg @ ProximityTerminalUseMessage(player_guid, object_guid, unk) =>
      log.info("ProximityTerminalUseMessage: " + msg)

    case msg @ MountVehicleMsg(player_guid, vehicle_guid, seat) =>
      log.info("MountVehicleMsg: " + msg)

    case msg @ DismountBuildingMsg(player_guid, building_guid) =>
      log.info("DismountBuildingMsg: " + msg)

    case msg @ GenericActionMessage(action) =>
      log.info("GenericActionMessage: " + msg)

    case msg @ HitHint(unk1, unk2, x) =>
      log.info("HitHint: " + msg)

    case msg @ WarpgateRequest(continent_guid, building_guid, dest_building_guid, dest_continent_guid, unk1, unk2) =>
      log.info("WarpgateRequest:" + msg)

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
}
