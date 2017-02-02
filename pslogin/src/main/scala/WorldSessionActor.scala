// Copyright (c) 2016 PSForever.net to present
//import java.net.{InetAddress, InetSocketAddress}

import net.psforever.packet.game.objectcreate._

import akka.actor.{Actor, ActorIdentity, ActorRef, Cancellable, Identify, MDCContextAware}
import net.psforever.packet.{PlanetSideGamePacket, _}
import net.psforever.packet.control._
import net.psforever.packet.game._
import scodec.Attempt.{Failure, Successful}
import scodec.bits._
import org.log4s.MDC
import MDCContextAware.Implicits._
import ServiceManager.Lookup
import net.psforever.types.{ChatMessageType, TransactionType, Vector3}

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, Try}

class WorldSessionActor extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger

  private case class PokeClient()

  var sessionId : Long = 0
  var leftRef : ActorRef = ActorRef.noSender
  var rightRef : ActorRef = ActorRef.noSender

  var serviceManager = Actor.noSender
  var chatService = Actor.noSender

  var useProximityTerminal = false
  var useProximityTerminalID = PlanetSideGUID(0)

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

      ServiceManager.serviceManager ! Lookup("chat")

      context.become(Started)
    case msg =>
      log.error(s"Unknown message ${msg}")
      context.stop(self)
  }

  def Started : Receive = {
    case ServiceManager.LookupResult(endpoint) =>
      chatService = endpoint
      log.debug("Got chat service " + endpoint)
    case ctrl @ ControlPacket(_, _) =>
      handlePktContainer(ctrl)
    case game @ GamePacket(_, _, _) =>
      handlePktContainer(game)
      // temporary hack to keep the client from disconnecting
    case PokeClient() =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))
    case ChatMessage(to, from, data) =>
      if(to.drop(6) == "local") sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_OPEN, true, from, data, None)))
      if(to.drop(6) == "squad") sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_SQUAD, true, from, data, None)))
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
//    println(pkt)
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
  //val objectHex = hex"18 57 0C 00 00 BC 84 B0 06 C2 D7 65 53 5C A1 60 00 01 34 40 00 09 70 49 00 6C 00 6C 00 6C 00 49 00 49 00 49 00 6C 00 6C 00 6C 00 49 00 6C 00 49 00 6C 00 6C 00 49 00 6C 00 6C 00 6C 00 49 00 6C 00 6C 00 49 00 84 52 70 76 1E 80 80 00 00 00 00 00 3F FF C0 00 00 00 20 00 00 0F F6 A7 03 FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FD 90 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00 00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46 86 C7 00 00 00 80 00 00 12 40 78 70 65 5F 73 61 6E 63 74 75 61 72 79 5F 68 65 6C 70 90 78 70 65 5F 74 68 5F 66 69 72 65 6D 6F 64 65 73 8B 75 73 65 64 5F 62 65 61 6D 65 72 85 6D 61 70 31 33 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02 60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80 00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41 C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00 00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05 48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32 00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90 00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8 05 26 40 00 03 20 06 C2 58 00 A7 88 00 00 02 00 00 80 00 00"
  //currently, the character's starting BEP is discarded due to unknown bit format
  val app = CharacterAppearanceData(
    Vector3(3674.8438f, 2726.789f, 91.15625f),
    32,
    0,
    false,
    4,
    "TestChar",
    0,
    2,
    2, 9,
    1,
    3, 118, 30, 0x8080, 0xFFFF, 2,
    0, 0, 7,
    RibbonBars(6,7,8,220)
  )
  val inv =
    InventoryItem(ObjectClass.TEMP730, PlanetSideGUID(76), 0, WeaponData(8, ObjectClass.BULLETS_9MM, PlanetSideGUID(77), 0, AmmoBoxData(20))) ::
//    InventoryItem(673, PlanetSideGUID(78), 2, WeaponData(8, 674, PlanetSideGUID(79), 0, AmmoBoxData(3))) ::
    InventoryItem(ObjectClass.TEMP556, PlanetSideGUID(78), 2, WeaponData(8, ObjectClass.BULLETS_9MM, PlanetSideGUID(79), 0, AmmoBoxData(100))) ::
    InventoryItem(ObjectClass.CHAIN_BLADE, PlanetSideGUID(80), 4, WeaponData(8, ObjectClass.FORCE_BLADE_AMMO, PlanetSideGUID(81), 0, AmmoBoxData(1))) ::
    InventoryItem(ObjectClass.SLOT_BLOCKER, PlanetSideGUID(82), 5, AmmoBoxData(1)) ::
    InventoryItem(ObjectClass.BUCKSHOT, PlanetSideGUID(83), 6, AmmoBoxData(25)) ::
    InventoryItem(ObjectClass.BULLETS_9MM, PlanetSideGUID(84), 9, AmmoBoxData(50)) ::
    InventoryItem(ObjectClass.BULLETS_9MM, PlanetSideGUID(85), 12, AmmoBoxData(50)) ::
    InventoryItem(ObjectClass.MEDKIT, PlanetSideGUID(91), 33, AmmoBoxData(1)) ::
    InventoryItem(ObjectClass.REK, PlanetSideGUID(88), 37, REKData(8)) ::
    InventoryItem(ObjectClass.MEDKIT, PlanetSideGUID(89), 51, AmmoBoxData(1)) ::
    InventoryItem(ObjectClass.MEDKIT, PlanetSideGUID(90), 69, AmmoBoxData(1)) ::
    InventoryItem(ObjectClass.BULLETS_9MM_AP, PlanetSideGUID(86), 64, AmmoBoxData(50)) ::
    InventoryItem(ObjectClass.PLASMA_GRENADE, PlanetSideGUID(92), 40, WeaponData(8, ObjectClass.PLASMA_GRENADE_AMMO, PlanetSideGUID(93), 0, AmmoBoxData(3))) ::
    InventoryItem(ObjectClass.PLASMA_GRENADE, PlanetSideGUID(94), 58, WeaponData(8, ObjectClass.PLASMA_GRENADE_AMMO, PlanetSideGUID(95), 0, AmmoBoxData(3))) ::
    InventoryItem(ObjectClass.PLASMA_GRENADE, PlanetSideGUID(96), 76, WeaponData(8, ObjectClass.PLASMA_GRENADE_AMMO, PlanetSideGUID(97), 0, AmmoBoxData(3))) ::
    Nil
  val obj = CharacterData(
    app,
    100, 77,
    88,
    1, 7, 7,
    100, 22,
    28, 4, 44, 84, 104, 1900,
    "xpe_sanctuary_help" :: "xpe_th_firemodes" :: "used_suppressor" :: "map12" :: Nil,
    // xpe_orbital_shuttle, xpe_drop_pod, xpe_sanctuary_help, xpe_warp_gate_usage, xpe_battle_rank_3, xpe_battle_rank_4, xpe_th_nonsanc, xpe_th_switchblade, xpe_th_router, xpe_th_flail, xpe_th_ant, xpe_th_ams, xpe_th_ground_p, xpe_th_air_p, xpe_th_hover, xpe_th_ground, xpe_th_bfr, xpe_th_afterburner, xpe_th_air, xpe_th_ammo, xpe_th_firemodes, xpe_th_cloak, xpe_th_max, used_oicw, used_advanced_ace, visited_ams, visited_ant, visited_lightgunship, visited_lightning, visited_magrider, visited_quadstealth, visited_skyguard, visited_two_man_assault_buggy, visited_twomanhoverbuggy, visited_flail, visited_router, visited_switchblade, visited_aurora, visited_fury, visited_quadassault, visited_apc_vs, visited_wasp, visited_aphelion_flight, visited_aphelion_gunner, visited_certification_terminal, visited_med_terminal, used_nano_dispenser, visited_broadcast_warpgate, used_ace, used_anniversary_gunb, used_aphelion_plasma_rocket_pod, used_aphelion_ppa, used_beamer, visited_bfr_terminal, used_bolt_driver, visited_mediumtransport, visited_galaxy_terminal, used_laze_pointer, used_flail_weapon, used_flamethrower, used_flechette, used_flux_cannon, used_fluxpod, used_ilc9, used_grenade_jammer, used_lancer, used_lasher, used_reaver_weapons, used_lightning_weapons, used_maelstrom, used_phoenix, used_grenade_plasma, used_pulsar, used_pulsed_particle_accelerator, used_punisher, used_radiator, used_rek, visited_deconstruction_terminal, used_rocklet, used_rotarychaingun, used_scythe, used_skyguard_weapons, used_spiker, used_suppressor, used_thumper, used_trek, visited_order_terminal, visited_ground_vehicle_terminal, used_vshev_comet, used_vshev_quasar, used_vshev_starfire, used_wasp_weapon_system, map15, map14, map13, map09, map07
    List.empty,
    InventoryData(
      true, false, false, inv
    )
  )
  val objectHex = ObjectCreateMessage(0, ObjectClass.AVATAR, PlanetSideGUID(75), obj)
  val objectHex2 = PacketCoding.EncodePacket(objectHex).require.toByteVector
  val objectHex4 = hex"1870ba0000bc80100d14d99dc06191200006e2400008604d006f006f006b0069006500620ed06201000000000000003fffce86800020000a00200302ac00000380000003cc0000019c0400002160b001b08c28000000000000000000000000019001900000000001007ec800b400000172d04dc00000000000000000000000000380004142c3440686c74909498a4a80101c1100007680000049007870655f636f6d6d616e645f72616e6b5f34927870655f636f6d6d616e645f72616e6b5f35927870655f626174746c655f72616e6b5f3232927870655f636f6d6d616e645f72616e6b5f338f7870655f666f726d5f6f7574666974907870655f666f726d5f706c61746f6f6e927870655f626174746c655f72616e6b5f3134927870655f626174746c655f72616e6b5f3135927870655f636f6d6d616e645f72616e6b5f32927870655f626174746c655f72616e6b5f3230927870655f626174746c655f72616e6b5f3138927870655f626174746c655f72616e6b5f3139927870655f626174746c655f72616e6b5f3137927870655f626174746c655f72616e6b5f3136927870655f626174746c655f72616e6b5f3235927870655f626174746c655f72616e6b5f3234927870655f626174746c655f72616e6b5f3233907870655f6a6f696e5f706c61746f6f6e927870655f626174746c655f72616e6b5f3133927870655f636f6d6d616e645f72616e6b5f31917870655f626174746c655f72616e6b5f36917870655f626174746c655f72616e6b5f37937870655f776172705f676174655f7573616765937870655f6f72626974616c5f73687574746c658c7870655f64726f705f706f64927870655f626174746c655f72616e6b5f31328f7870655f6a6f696e5f6f75746669748c7870655f62696e645f616d738e7870655f666f726d5f7371756164917870655f626174746c655f72616e6b5f39927870655f626174746c655f72616e6b5f3131917870655f626174746c655f72616e6b5f38927870655f696e7374616e745f616374696f6e8e7870655f6a6f696e5f7371756164917870655f626174746c655f72616e6b5f34917870655f626174746c655f72616e6b5f32917870655f62696e645f666163696c697479927870655f73616e6374756172795f68656c70927870655f626174746c655f72616e6b5f3130917870655f626174746c655f72616e6b5f35917870655f626174746c655f72616e6b5f33907870655f6f766572686561645f6d61708d7870655f776172705f67617465927870655f74685f737769746368626c6164658a7870655f74685f616e748a7870655f74685f62667289757365645f6f69637791757365645f616476616e6365645f61636597766973697465645f73706974666972655f74757272657498766973697465645f73706974666972655f636c6f616b656493766973697465645f73706974666972655f616192766973697465645f74616e6b5f7472617073a1766973697465645f706f727461626c655f6d616e6e65645f7475727265745f6e63a1766973697465645f706f727461626c655f6d616e6e65645f7475727265745f7472a1766973697465645f706f727461626c655f6d616e6e65645f7475727265745f76738e757365645f6d616763757474657293766973697465645f77616c6c5f74757272657498766973697465645f616e6369656e745f7465726d696e616c8b766973697465645f616d738b766973697465645f616e7490766973697465645f64726f707368697091766973697465645f6c6962657261746f7294766973697465645f6c6967687467756e7368697091766973697465645f6c696768746e696e6790766973697465645f6d616772696465728f766973697465645f70726f776c657293766973697465645f71756164737465616c746890766973697465645f736b7967756172649a766973697465645f74687265656d616e686561767962756767799d766973697465645f74776f5f6d616e5f61737361756c745f627567677998766973697465645f74776f6d616e6865617679627567677998766973697465645f74776f6d616e686f766572627567677990766973697465645f76616e67756172648d766973697465645f666c61696c8e766973697465645f726f7574657293766973697465645f737769746368626c6164658e766973697465645f6175726f726193766973697465645f626174746c657761676f6e8c766973697465645f6675727993766973697465645f7175616461737361756c7496766973697465645f67616c6178795f67756e736869708e766973697465645f6170635f74728e766973697465645f6170635f767390766973697465645f6c6f64657374617290766973697465645f7068616e7461736d91766973697465645f7468756e64657265728e766973697465645f6170635f6e638f766973697465645f76756c747572658c766973697465645f7761737090766973697465645f6d6f73717569746f97766973697465645f617068656c696f6e5f666c6967687497766973697465645f617068656c696f6e5f67756e6e657297766973697465645f636f6c6f737375735f666c6967687497766973697465645f636f6c6f737375735f67756e6e657298766973697465645f706572656772696e655f666c6967687498766973697465645f706572656772696e655f67756e6e657289757365645f62616e6b95766973697465645f7265736f757263655f73696c6f9e766973697465645f63657274696669636174696f6e5f7465726d696e616c94766973697465645f6d65645f7465726d696e616c93757365645f6e616e6f5f64697370656e73657295766973697465645f73656e736f725f736869656c649a766973697465645f62726f6164636173745f77617270676174658c757365645f7068616c616e7896757365645f7068616c616e785f666c616b636f6d626f96766973697465645f77617270676174655f736d616c6c88757365645f61636598766973697465645f6164765f6d65645f7465726d696e616c8d757365645f66697265626972649c766973697465645f6169725f76656869636c655f7465726d696e616c95757365645f616e6e69766572736172795f67756e6195757365645f616e6e69766572736172795f67756e6294757365645f616e6e69766572736172795f67756e8c757365645f62616c6c67756e90757365645f37356d6d5f63616e6e6f6e92757365645f6170635f6e635f776561706f6e92757365645f6170635f76735f776561706f6e9f757365645f617068656c696f6e5f706c61736d615f726f636b65745f706f6491757365645f617068656c696f6e5f7070618b757365645f6265616d657294766973697465645f6266725f7465726d696e616c90757365645f626f6c745f647269766572a0757365645f636f6c6f737375735f6475616c5f3130306d6d5f63616e6e6f6e7399757365645f636f6c6f737375735f74616e6b5f63616e6e6f6e93757365645f636f6d6d616e645f75706c696e6b96766973697465645f656e657267795f6372797374616c96766973697465645f6865616c74685f6372797374616c96766973697465645f7265706169725f6372797374616c8b757365645f6379636c657297766973697465645f6d656469756d7472616e73706f727498766973697465645f736869656c645f67656e657261746f7297766973697465645f67616c6178795f7465726d696e616c92757365645f656e657267795f67756e5f6e6392757365645f656e657267795f67756e5f747292757365645f656e657267795f67756e5f767391757365645f6c617a655f706f696e74657291757365645f666c61696c5f776561706f6e91757365645f666c616d657468726f7765728e757365645f666c6563686574746590757365645f666c75785f63616e6e6f6e8c757365645f666c7578706f6491757365645f6772656e6164655f6672616798757365645f33356d6d5f726f74617279636861696e67756e8a757365645f67617573739a766973697465645f67656e657261746f725f7465726d696e616c9b757365645f68656176795f6772656e6164655f6c61756e6368657294757365645f68656176795f7261696c5f6265616d91757365645f68656176795f736e6970657291757365645f68756e7465727365656b657289757365645f696c633998766973697465645f696d706c616e745f7465726d696e616c88757365645f69737093757365645f6772656e6164655f6a616d6d65728b757365645f6b6174616e618b757365645f6c616e6365728b757365645f6c617368657290757365645f32356d6d5f63616e6e6f6e99757365645f6c6962657261746f725f626f6d6261726469657290757365645f33356d6d5f63616e6e6f6e93757365645f7265617665725f776561706f6e7396757365645f6c696768746e696e675f776561706f6e7392766973697465645f4c4c555f736f636b65749a766973697465645f65787465726e616c5f646f6f725f6c6f636b8e757365645f6d61656c7374726f6d98766973697465645f7265737061776e5f7465726d696e616c8e766973697465645f6c6f636b65728c757365645f6d65645f61707090757365645f32306d6d5f63616e6e6f6e92757365645f6d696e695f636861696e67756e99766973697465645f6d6f6e6f6c6974685f636572797368656e97766973697465645f6d6f6e6f6c6974685f6573616d697299766973697465645f6d6f6e6f6c6974685f666f72736572616c95766973697465645f6d6f74696f6e5f73656e736f7292757365645f6e635f6865765f66616c636f6e99757365645f6e635f6865765f7363617474657263616e6e6f6e93757365645f6e635f6865765f73706172726f779f757365645f706572656772696e655f6475616c5f6d616368696e655f67756e9f757365645f706572656772696e655f6475616c5f726f636b65745f706f64739e757365645f706572656772696e655f7061727469636c655f63616e6e6f6e8c757365645f70686f656e697893757365645f6772656e6164655f706c61736d6191757365645f3130356d6d5f63616e6e6f6e92757365645f31356d6d5f636861696e67756e8b757365645f70756c7361728d757365645f70756e69736865728e757365645f725f73686f7467756e8d757365645f7261646961746f7288757365645f72656b93766973697465645f7265706169725f73696c6f8d757365645f72657065617465729f766973697465645f6465636f6e737472756374696f6e5f7465726d696e616c8c757365645f726f636b6c657493757365645f726f74617279636861696e67756e8c757365645f74656c657061648b757365645f73637974686599766973697465645f7365636f6e646172795f6361707475726595757365645f736b7967756172645f776561706f6e7391766973697465645f67656e657261746f728b757365645f7370696b65728c757365645f737472696b65728f757365645f73757070726573736f728c757365645f7468756d70657291757365645f67617573735f63616e6e6f6e89757365645f7472656b95757365645f76616e67756172645f776561706f6e73a4766973697465645f616e6369656e745f6169725f76656869636c655f7465726d696e616c9c766973697465645f76616e755f636f6e74726f6c5f636f6e736f6c6598766973697465645f636170747572655f7465726d696e616ca2766973697465645f616e6369656e745f65717569706d656e745f7465726d696e616c96766973697465645f6f726465725f7465726d696e616c93766973697465645f76616e755f6d6f64756c659a757365645f616e6369656e745f7475727265745f776561706f6ea7766973697465645f616e6369656e745f67726f756e645f76656869636c655f7465726d696e616c9f766973697465645f67726f756e645f76656869636c655f7465726d696e616c97757365645f76756c747572655f626f6d6261726469657298757365645f76756c747572655f7461696c5f63616e6e6f6e857567643036857567643035857567643033857567643032856d61703939856d61703938856d61703937856d61703936856d61703135856d61703134856d61703131856d61703130856d61703039856d61703038856d61703037856d61703036856d61703035856d61703034856d61703033856d61703032856d617030310000000000000000000000000000000000000000000000008000000041070800100402400000100004d4206010890000004000180838810020320000008019041402172000000804f0da40c01019000000400018083801c0203200000a001b4820020f2000000800030107004804064000014003690500436400000100006020e00b0080c8000028006d20c0089c80000020000c041c01a01019000005000da41c01199000000400018083803c0203200000a001b4840023f2000000800030107008804064000014003690900496400000100006020e0130080c8000028006d2140095c80000020000c041c02a01019000005000da42c01319000000400018083805c0203200000a001b4860026f200000080003010700c804064000014003690d006d6400000100006020e01b0080c8000028006d21c00ddc80000020000c041c03a01019000005000da43c01c19000000400018083807c0203200000a001b4880038f2000000800030107010804064000014003691100736400000100006020e0230080c8000028006d22400e9c80000020000c041c04a01019000005000da44c01d99000000400018083809c0203200000a001b48a003bf2000000800030107014804064000014003691500796400000100006020e02b0080c8000008006d22c00f5c80000020000c041c05a01019000005000da45c000169900000040001808380bc0203200000a001b48c00002df2000000800030107018804064000014003691900005d6400000100006020e0330080c8000028006d2340000bdc80000020000c041c06a01019000005000da46c000181900000040001808380dc02032000004001b48e000030f200000080003010701c804064000014003691d0000636400000100006020e03b0080c8000028006d23c0000c9c80000020000c041c07a01019000005000da47c000199900000040001808380fc0203200000a001b490000033f200000080003010702080406400001400369210000876400000100006020e0430080c8000028006d244000111c80000020000c041c08a01019000005000da48c0002299000000400018083811c0203200000a001b492000045f2000000800030107024804064000014003692500008d6400000100006020e04b0080c8000028006d24c00011dc80000020000c041c09a01019000005000da49c0002419000000400018083813c0203200000a001b494000048f200000080003010702880406400001400369290000936400000100006020e0530080c8000028006d254000129c80000020000c041c0aa01019000005000da4ac0002d19000000400018083815c0203200000a001b49600005af200000080003010702c804064000014003692d0000b76400000100006020e05b0080c8000028006d25c000171c80000020000c041c0ba01019000005000da4bc0002e99000000400018083817c02032000005801b49800005df200000080003010703080406400001400369310000bd6400000100006020e0630080c8000000006d26400017dc80000020000c041c0ca01019000005000da4cc0003019000000400018083819c02032000004001b49a000060f200000080003010703480406400001400369350000e16400000100006020e06b0080c8000028006d26c0001c5c80000020000c041c0da01019000005000da4dc000391900000040001808381bc0203200000a001b49c000072f200000080003010703880406400001400369390000e76400000100006020e0730080c8000028006d2740001d1c80000020000c041c0ea01019000005000da4ec0003a9900000040001808381dc0203200000a001b49e000075f200000080003010703c804064000014003693d0000ed6400000100006020e07b0080c8000028006d27c0001ddc80000020000c041c0fa01019000005000200fc010c9000000400013613f8044a4000001000040c0020100013090000004000108040804da400000100004e"

  val traveler = Traveler(this, "home2")

  def handleGamePkt(pkt : PlanetSideGamePacket) = pkt match {
    case ConnectToWorldRequestMessage(server, token, majorVersion, minorVersion, revision, buildDate, unk) =>

      val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"

      log.info(s"New world login to ${server} with Token:${token}. ${clientVersion}")

      // ObjectCreateMessage
//      sendResponse(PacketCoding.CreateGamePacket(0,ObjectCreateMessage(3159,121,PlanetSideGUID(75),None,Some(CharacterData(CharacterAppearanceData(Vector3(3674.8438f,2726.789f,91.15625f),19,2,false,4,"IlllIIIlllIlIllIlllIllI",1,2,2,9,1,3,118,30,32896,65535,2,255,106,7,RibbonBars(4294,2949,9496,4967)),100,100,50,1,7,7,100,100,28,4,44,84,104,1900,List("xpe_sanctuary_help", "xpe_th_firemodes", "used_beamer", "map13"),List(),InventoryData(true,false,false,List(InventoryItem(InternalSlot(140,PlanetSideGUID(76),0,WeaponData(8,InternalSlot(272,PlanetSideGUID(77),0,AmmoBoxData(16))))), InventoryItem(InternalSlot(845,PlanetSideGUID(78),2,WeaponData(8,InternalSlot(28,PlanetSideGUID(79),0,AmmoBoxData(25))))), InventoryItem(InternalSlot(324,PlanetSideGUID(80),4,WeaponData(8,InternalSlot(540,PlanetSideGUID(81),0,AmmoBoxData(1))))), InventoryItem(InternalSlot(456,PlanetSideGUID(82),5,AmmoBoxData(1))), InventoryItem(InternalSlot(28,PlanetSideGUID(83),6,AmmoBoxData(50))), InventoryItem(InternalSlot(28,PlanetSideGUID(84),9,AmmoBoxData(50))), InventoryItem(InternalSlot(28,PlanetSideGUID(85),12,AmmoBoxData(50))), InventoryItem(InternalSlot(29,PlanetSideGUID(86),33,AmmoBoxData(50))), InventoryItem(InternalSlot(272,PlanetSideGUID(87),36,AmmoBoxData(50))), InventoryItem(InternalSlot(728,PlanetSideGUID(88),39,REKData(8))))))))))
      sendResponse(PacketCoding.CreateGamePacket(0, objectHex))

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
          objectHex match {
            case obj @ ObjectCreateMessage(len, cls, guid, _, _) =>
              log.debug("Object: " + obj)
              // LoadMapMessage 13714 in mossy .gcap
              // XXX: hardcoded shit
              sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))

              val home2 = Zone.get("home2").get
              Transfer.loadMap(traveler, home2)
              Transfer.loadSelf(traveler, Zone.selectRandom(home2))
//              sendResponse(PacketCoding.CreateGamePacket(0, BattleExperienceMessage(guid,100000000,0)))
              sendResponse(PacketCoding.CreateGamePacket(0,ChatMsg(ChatMessageType.CMT_OPEN,true,"", "Welcome! The commands '/zone' and '/warp' are available for use.", None)))
              sendResponse(PacketCoding.CreateGamePacket(0,ChatMsg(ChatMessageType.CMT_OPEN,true,"", "Change cont will reset your inventory !", None)))
              sendResponse(PacketCoding.CreateGamePacket(0,ChatMsg(ChatMessageType.CMT_EXPANSIONS,true,"", "1 on", None)))

              // test AvatarStatisticsMessage
              sendRawResponse(hex"000901f80019067f4000000000247f013c400000000000000000000000000000000000000000200000000000000000000000247f013d600000000020000000000000000000000000000000000000000000000000000000067f4000000000247f024d600000000020000000000000000000000000000000200000000000000000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000067f4000000000")
              sendResponse(PacketCoding.CreateGamePacket(0,FavoritesMessage(0,PlanetSideGUID(75),0,"@fav_light_infantry",Some(1))))
              // test OrbitalShuttleTimeMsg
              sendRawResponse(hex"5b75c4020180200f8000583a80000a80e041142903820450a00e0c1140")
              sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),2,61)))
              sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),2,60)))



              //              sendResponse(PacketCoding.CreateGamePacket(0, objectHex))

              // These object_guids are specfic to VS Sanc
//              for(nanototo <- 0 to 1024)
//                sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(nanototo), PlanetSideEmpire.TR)))
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.TR))) //HART building C
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.TR))) //South Villa Gun Tower
//              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(1397), PlanetSideEmpire.TR)))

              sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(1191182336)))
              sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
              sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), 32))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate

//              //Little job to load some data from gcap files
//              import scala.io.Source
//              val bufferedFile = Source.fromFile(".\\pslogin\\src\\main\\scala\\BuildingInfoUpdateMessage.txt")
//              for (line <- bufferedFile.getLines){
//                val SomeDataFromFile = line.split(',')
//                sendResponse(PacketCoding.CreateGamePacket(0,BuildingInfoUpdateMessage(
//                  PlanetSideGUID(SomeDataFromFile{0}.toInt),
//                  PlanetSideGUID(SomeDataFromFile{1}.toInt),
//                  SomeDataFromFile{2}.toInt,
//                  SomeDataFromFile{3}.toBoolean,
//                  PlanetSideEmpire(SomeDataFromFile{4}.toInt),
//                  SomeDataFromFile{5}.toInt,
//                  PlanetSideEmpire(SomeDataFromFile{6}.toInt),
//                  SomeDataFromFile{7}.toInt,
//                  PlanetSideGeneratorState(SomeDataFromFile{8}.toInt),
//                  SomeDataFromFile{9}.toBoolean,
//                  SomeDataFromFile{10}.toBoolean,
//                  SomeDataFromFile{11}.toInt,
//                  SomeDataFromFile{12}.toInt,
//                  SomeDataFromFile{13}.toInt,
//                  SomeDataFromFile{14}.toInt,
//                  SomeDataFromFile{15}.toBoolean,
//                  SomeDataFromFile{16}.toInt,
//                  SomeDataFromFile{17}.toBoolean,
//                  SomeDataFromFile{18}.toBoolean)))
//              }
//              bufferedFile.close()

              sendResponse(PacketCoding.CreateGamePacket(0,BuildingInfoUpdateMessage(
                PlanetSideGUID(6),   //Ceryshen
                PlanetSideGUID(2),   //Anguta
                8,                   //80% NTU
                true,                //Base hacked
                PlanetSideEmpire.NC, //Base hacked by NC
                600000,              //10 minutes remaining for hack
                PlanetSideEmpire.VS, //Base owned by VS
                0,                   //!! Field != 0 will cause malformed packet. See class def.
                None,
                PlanetSideGeneratorState.Critical, //Generator critical
                true,                //Respawn tubes destroyed
                true,                //Force dome active
                16,                  //Tech plant lattice benefit
                0,
                Nil,                   //!! Field > 0 will cause malformed packet. See class def.
                0,
                false,
                8,                   //!! Field != 8 will cause malformed packet. See class def.
                None,
                true,                //Boosted spawn room pain field
                true)))              //Boosted generator room pain field

              sendResponse(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(guid,0,0)))

              chatService ! ChatService.Join("local")
              chatService ! ChatService.Join("squad")

              sendResponse(PacketCoding.CreateGamePacket(0, CreateShortcutMessage(guid, 1, 0, true, Shortcut.MEDKIT)))

              import scala.concurrent.duration._
              import scala.concurrent.ExecutionContext.Implicits.global
              clientKeepAlive = context.system.scheduler.schedule(0 seconds, 500 milliseconds, self, PokeClient())
          }
        case default =>
          log.error("Unsupported " + default + " in " + msg)
      }
    case msg @ CharacterCreateRequestMessage(name, head, voice, gender, empire) =>
      log.info("Handling " + msg)

      sendResponse(PacketCoding.CreateGamePacket(0,ObjectCreateMessage(3159,121,PlanetSideGUID(100),None,Some(CharacterData(CharacterAppearanceData(Vector3(3674.8438f,2726.789f,91.15625f),19,empire.id,false,4,name,0,gender.id,2,9,1,3,118,30,32896,65535,2,255,106,7,RibbonBars()),
        100,90,75,1,7,7,100,100,28,4,44,84,104,1900,
        List(),
        List(),
        InventoryData(true,false,false,List()))))))

      sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(true, None)))

      sendResponse(PacketCoding.CreateGamePacket(0,CharacterInfoMessage(PlanetSideZoneID(10000),41605313,PlanetSideGUID(75),false,6404428)))
      sendResponse(PacketCoding.CreateGamePacket(0,CharacterInfoMessage(PlanetSideZoneID(10000),41605314, PlanetSideGUID(100), true, 0)))

    case KeepAliveMessage(code) =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))

    case msg @ PlayerStateMessageUpstream(avatar_guid, pos, vel, unk1, aim_pitch, unk2, seq_time, unk3, is_crouching, unk4, unk5, is_cloaking, unk6, unk7) =>
//      log.info("PlayerState: " + msg)
      if(useProximityTerminal == true && vel == None){
        sendResponse(PacketCoding.CreateGamePacket(0,ProximityTerminalUseMessage(avatar_guid, useProximityTerminalID, true)))
      }
      if(useProximityTerminal == true && vel != None) {
        useProximityTerminal = false
        sendResponse(PacketCoding.CreateGamePacket(0,ProximityTerminalUseMessage(PlanetSideGUID(0), useProximityTerminalID, false)))
      }

    case msg @ ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
        log.info("Chat: " + msg)
      }

//      if(messagetype == ChatMessageType.CMT_OPEN) {
//        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(4717), 0)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(5398), 0)))
//        val msg = ObjectCreateMessage(0,contents.toInt,PlanetSideGUID(4717),Some(ObjectCreateMessageParent(PlanetSideGUID(75),1)),Some(WeaponData(0,InternalSlot(681,PlanetSideGUID(5398),0,AmmoBoxData(500)))))
//        val pkt = PacketCoding.EncodePacket(msg).require.toByteVector
//        sendRawResponse(pkt)
//      }

//      if(messagetype == ChatMessageType.CMT_TELL) {
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_223,true,"","@CTF_Failed_SourceResecured^@TerranRepublic~^@Hanish~",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_224,false,"","@TooFastToDismount",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_225,false,"","@DoorWillOpenWhenShuttleReturns",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@ArmorShieldOverride",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@charsaved",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@SVCP_PositionInQueue^1~^1~",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@ArmorShieldOff",None)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),83,20)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),84,20)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),85,10)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),86,10)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),87,20)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),88,5)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),89,5)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),90,1)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),91,5)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),92,5)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),93,20)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),94,1)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),95,10)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),96,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),97,4)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),98,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),99,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),100,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),101,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),102,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),103,0)))
//        sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(PlanetSideGUID(75),104,0)))
//      }

      if (messagetype == ChatMessageType.CMT_VOICE) {
        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_VOICE, false, "TestChar", contents,None)))
        sendResponse(PacketCoding.CreateGamePacket(0, BattleExperienceMessage(PlanetSideGUID(75),1500,0)))
      }
      if (messagetype == ChatMessageType.CMT_WHO || messagetype == ChatMessageType.CMT_WHO_CSR || messagetype == ChatMessageType.CMT_WHO_CR ||
        messagetype == ChatMessageType.CMT_WHO_PLATOONLEADERS || messagetype == ChatMessageType.CMT_WHO_SQUADLEADERS || messagetype == ChatMessageType.CMT_WHO_TEAMS) {
        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_WHO, true,"", "That command doesn't work for now",None)))
      }

      CSRZone.read(traveler, msg)
      CSRWarp.read(traveler, msg)

      // TODO: handle this appropriately
      if(messagetype == ChatMessageType.CMT_QUIT) {
        sendResponse(DropCryptoSession())
        sendResponse(DropSession(sessionId, "user quit"))
      }

      chatService ! ChatService.NewMessage("Player_ID_" + sessionId.toString, msg)

      // TODO: Depending on messagetype, may need to prepend sender's name to contents with proper spacing
      // TODO: Just replays the packet straight back to sender; actually needs to be routed to recipients!
      sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))

    case msg @ VoiceHostRequest(unk, PlanetSideGUID(player_guid), data) =>
      log.info("Player "+player_guid+" requested in-game voice chat.")
      sendResponse(PacketCoding.CreateGamePacket(0, VoiceHostKill()))

    case msg @ VoiceHostInfo(player_guid, data) =>
      sendResponse(PacketCoding.CreateGamePacket(0, VoiceHostKill()))

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
      sendResponse(PacketCoding.CreateGamePacket(0, DropItemMessage(item_guid)))

    case msg @ ReloadMessage(item_guid, ammo_clip, unk1) =>
//      log.info("Reload: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ReloadMessage(item_guid, 100, unk1)))

    case msg @ ObjectHeldMessage(avatar_guid, held_holsters, unk1) =>
      log.info("ObjectHeld: " + msg)

    case msg @ AvatarJumpMessage(state) =>
      log.info("AvatarJump: " + msg)

    case msg @ ZipLineMessage(player_guid,origin_side,action,id,unk3,unk4,unk5) =>
      log.info("ZipLineMessage: " + msg)
      if(action == 0) {
        sendResponse(PacketCoding.CreateGamePacket(0,ZipLineMessage(player_guid,origin_side,action,id,unk3,unk4,unk5)))
      }
      else if(action == 1) {
        //disembark from zipline at destination?
      }
      else if(action == 2) {
        //get off by force
      }

    case msg @ RequestDestroyMessage(object_guid) =>
      log.info("RequestDestroy: " + msg)
      // TODO: Make sure this is the correct response in all cases
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg @ ObjectDeleteMessage(object_guid, unk1) =>
      log.info("ObjectDelete: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg @ MoveItemMessage(item_guid, avatar_guid_1, avatar_guid_2, dest, unk1) =>
      log.info("MoveItem: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(avatar_guid_1,item_guid,dest)))

    case msg @ ChangeAmmoMessage(item_guid, unk1) =>
      log.info("ChangeAmmo: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(item_guid, unk1)))

    case msg @ UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9) =>
      log.info("UseItem: " + msg)
      // TODO: Not all fields in the response are identical to source in real packet logs (but seems to be ok)
      // TODO: Not all incoming UseItemMessage's respond with another UseItemMessage (i.e. doors only send out GenericObjectStateMsg)
      sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9)))
      if(unk1 != 0){ // TODO : medkit use ?!
        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(unk1), 2)))
      } else {
        // TODO: This should only actually be sent to doors upon opening; may break non-door items upon use
        sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(object_guid, 16)))
      }

    case msg @ GenericObjectStateMsg(object_guid, unk1) =>
      log.info("GenericObjectState: " + msg)

    case msg @ ItemTransactionMessage(terminal_guid, transaction_type, item_page, item_name, unk1, item_guid) =>
      log.info("ItemTransaction: " + msg)
      if(transaction_type == TransactionType.Sell) {
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(item_guid, 0)))
      }
      if(transaction_type == TransactionType.Buy) {
        val obj = AmmoBoxData(50)
        val msg = ObjectCreateMessage(0, 28, PlanetSideGUID(1280), ObjectCreateMessageParent(PlanetSideGUID(75), 33), obj)
        val pkt = PacketCoding.EncodePacket(msg).require.toByteVector
        sendRawResponse(pkt)
      }
//      if(transaction_type == TransactionType.Learn && item_name == "anti_vehicular") {
//        sendRawResponse(hex"45e4003000")
//      }

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
	  
    case msg @ GenericActionMessage(action) =>
      log.info("GenericActionMessage: " + msg)

    case msg @ MountVehicleMsg(player_guid, vehicle_guid, seat) =>
      log.info("MountVehicleMsg: "+msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(vehicle_guid,player_guid,0)))

    case msg @ SplashHitMessage(bytes) =>
      log.info("SplashHitMessage: " + bytes.toString)

    case msg @ WarpgateRequest(continent_guid, building_guid, dest_building_guid, dest_continent_guid, unk1, unk2) =>
      log.info("WarpgateRequest: " + msg)

    case msg @ GenericCollisionMsg(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
      log.info("GenericCollision: "+msg)

    case msg @ PlanetsideAttributeMessage(avatar_guid, unk2, unk3) =>
      log.info("PlanetsideAttributeMessage: "+msg)
      sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(avatar_guid, unk2, unk3)))

    case msg @ ProximityTerminalUseMessage(player_guid, object_guid, unk) =>
      log.info("ProximityTerminalUseMessage: "+msg)
      if(unk == false){
        useProximityTerminal = true
        useProximityTerminalID = object_guid
      }

    case default =>
      log.debug(s"Unhandled GamePacket ${pkt}")
      log.info(s"unk: ${pkt}")
  }

  def failWithError(error : String) = {
    log.error(error)
    sendResponse(PacketCoding.CreateControlPacket(ConnectionClose()))
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

  /**
    * `sendToSelf` is an external call that permits the session to gain access to `rightRef` so that it can dispatch a packet.
    * @param msg a packet
    */
  def sendToSelf(msg : PlanetSidePacketContainer) : Unit = {
    sendResponse(msg)
  }
  /**
    * `sendToSelf` is an external call that permits the session to gain access to `rightRef` so that it can dispatch a packet.
    * @param msg the byte-code translation of a packet
    */
  def sendToSelf(msg : ByteVector) : Unit = {
    sendRawResponse(msg)
  }
}

//----------------------------------------------------------------------------------------------------------------------
/*
The following is for development and fun.
The code is placed herein this file because of difficulty accessing WorldSessionActor from other packages.
Once the aforementioned class has been refactored, these classes can be moved.
Eventually, they should be removed or replaced altogether.
 */
/**
  * The traveler is synonymous with the player.
  * The primary purpose of the object is to keep track of but not expose the player's session so that packets may be relayed back to him.
  * Traveler also keeps track of which zone the player currently occupies.
  * @param session the player's session
  */
class Traveler(private val session : WorldSessionActor) {
  /**
    * The byte-code form a a CreateObjectMessage that would construct the player's avatar
    */
  val player : ByteVector = session.objectHex2

  /**
    * The name of the zone the player currently occupies
    */
  var zone : String = ""

  /**
    * `sendToSelf` is a call that permits the session to gain access to its internal `rightRef` so that it can dispatch a packet.
    * @param msg a packet
    */
  def sendToSelf(msg : ByteVector) : Unit = {
    this.session.sendRawResponse(msg)
  }

  /**
    * `sendToSelf` is a call that permits the session to gain access to its internal `rightRef` so that it can dispatch a packet.
    * @param msg the byte-code translation of a packet
    */
  def sendToSelf(msg : PlanetSidePacketContainer) : Unit = {
    this.session.sendResponse(msg)
  }
}

object Traveler {
  /**
    * An abbreviated constructor for creating `Traveler`s without invocation of `new`.
    * @param session the player's session
    * @return a traveler object for this player
    */
  def apply(session : WorldSessionActor) : Traveler = new Traveler(session)

  /**
    * An abbreviated constructor for creating `Traveler`s without invocation of `new`, and for assigning a default zone.
    * @param session the player's session
    * @param zoneId the zone the player currently occupies
    * @return a traveler object for this player
    */
  def apply(session : WorldSessionActor, zoneId : String) : Traveler = {
    val traveler = new Traveler(session)
    traveler.zone = zoneId
    traveler
  }
}

/**
  * An implementation of the CSR command `/zone`, slightly modified to serve the purposes of the testing phases of the server.
  */
object CSRZone {
  /**
    * Accept and confirm that a message sent to a player is a valid `/zone` invocation.
    * If so, parse the message and send the player to whichever zone was requested.
    * @param traveler the player
    * @param msg the message the player received
    * @return true, if the player is being transported to another zone; false, otherwise
    */
  def read(traveler : Traveler, msg : ChatMsg) : Boolean = {
    if(!isProperRequest(msg))
      return false  //we do not handle this message

    val buffer = decomposeMessage(msg.contents)
    if(buffer.length == 0 || buffer(0).equals("-help")) {
      CSRZone.help(traveler) //print usage information to chat
      return false
    }

    var zoneId = ""
    var gateId = "" //the user can define which warpgate they may visit (actual keyword protocol missing)
    var list = false //if the user wants a printed list of destination locations
    for(o <- buffer) {
      if(o.equals("-list")) {
        if(zoneId.equals("") || gateId.equals("")) {
          list = true
        }
      }
      else if(zoneId.equals(""))
        zoneId = o
      else if(gateId.equals(""))
        gateId = o
    }

    val zoneOpt = Zone.get(zoneId)
    if(zoneOpt.isEmpty) {
      if(list)
        CSRZone.reply(traveler, Zone.list)
      else
        CSRZone.error(traveler, "Give a valid zonename (use '/zone -list')")
      return false
    }
    val zone = zoneOpt.get
    traveler.zone = zoneId
    var destination : (Int, Int, Int) = Zone.selectRandom(zone) //the destination in the new zone starts as random

    if(!gateId.equals("")) { //if we've defined a warpgate, and can find that warpgate, we re-assign the destination
      val gateOpt = Zone.getWarpgate(zone, gateId)
      if(gateOpt.isDefined)
        destination = gateOpt.get
      else
        CSRZone.error(traveler, "Gate id not defined (use '/zone <zone> -list')")
    }
    else if(list) {
      CSRZone.reply(traveler, Zone.listWarpgates(zone))
      return false
    }
    Transfer.zone(traveler, zone, destination)
    true
  }

  /**
    * Check that the incoming message is an appropriate type for this command.
    * @param msg the message
    * @return true, if we will handle it; false, otherwise
    */
  def isProperRequest(msg : ChatMsg) : Boolean ={
    msg.messageType == ChatMessageType.CMT_ZONE
  }

  /**
    * Break the message in the packet down for parsing.
    * @param msg the contents portion of the message, a space-separated `String`
    * @return the contents portion of the message, transformed into an `Array`
    */
  private def decomposeMessage(msg : String) : Array[String] = {
    msg.trim.toLowerCase.split("\\s+")
  }

  /**
    * Send a message back to the `Traveler` that will be printed into his chat window.
    * @param traveler the player
    * @param msg the message to be sent
    */
  private def reply(traveler : Traveler, msg : String) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", msg, None))
    )
  }

  /**
    * Print usage information to the `Traveler`'s chat window.
    * @param traveler the player
    */
  private def help(traveler : Traveler) : Unit = {
    CSRZone.reply(traveler, "usage: /zone <zone> [gatename] | [-list]")
  }

  /**
    * Print error information to the `Traveler`'s chat window.<br>
    * The most common reason for error is the lack of information, or wrong information.
    * @param traveler the player
    */
  private def error(traveler : Traveler, msg : String) : Unit = {
    CSRZone.reply(traveler, "Error! "+msg)
  }
}

/**
  * An implementation of the CSR command `/warp`, highly modified to serve the purposes of the testing phases of the server.
  * See `help()` for details.
  */
object CSRWarp {
  /**
    * Accept and confirm that a message sent to a player is a valid `/warp` invocation.
    * If so, parse the message and send the player to whichever destination in this zone was requested.
    * @param traveler the player
    * @param msg the message the player received
    * @return true, if the player is being transported to another place; false, otherwise
    */
  def read(traveler : Traveler, msg : ChatMsg) : Boolean = {
    if(!isProperRequest(msg))
      return false //we do not handle this message

    val buffer = decomposeMessage(msg.contents)
    if(buffer.length == 0 || buffer(0).equals("") || buffer(0).equals("-help")) {
      CSRWarp.help(traveler) //print usage information to chat
      return false
    }

    var destId : String = ""
    var coords : ArrayBuffer[Int] = ArrayBuffer.empty[Int]
    var list : Boolean = false
    var failedCoordInput = false
    for(o <- buffer) {
      val toInt = Try(o.toInt)
      if(toInt.isSuccess) {
        coords += toInt.get
      }
      else if(coords.nonEmpty && coords.size < 3)
        failedCoordInput = true
      if(o.equals("-list"))
        list = true
      else if(destId.equals(""))
        destId = o
    }

    if(failedCoordInput || (coords.nonEmpty && coords.size < 3)) {
      CSRWarp.error(traveler, "Needs three integer components (<x> <y> <z>)")
      return false
    }
    else {
      coords.slice(0, 3).foreach( x => {
        if(x < 0 || x > 8191) {
          CSRWarp.error(traveler, "Out of range - 0 < n < 8191, but n = "+x)
          return false
       }
      })
    }

    val zone = Zone.get(traveler.zone).get //the traveler is already in the appropriate zone
    if(list && coords.isEmpty && destId.equals("")) {
      CSRWarp.reply(traveler, Zone.listLocations(zone)+"; "+Zone.listWarpgates(zone))
      return false
    }
    val dest : Option[(Int, Int, Int)] = if(coords.nonEmpty) Some(coords(0), coords(1), coords(2)) else Zone.getWarpLocation(zone, destId) //coords before destId
    if(dest.isEmpty) {
      CSRWarp.error(traveler, "Invalid location")
      return false
    }
    Transfer.warp(traveler, dest.get)
    true
  }

  /**
    * Check that the incoming message is an appropriate type for this command.
    * @param msg the message
    * @return true, if we will handle it; false, otherwise
    */
  def isProperRequest(msg : ChatMsg) : Boolean = {
    msg.messageType == ChatMessageType.CMT_WARP
  }

  /**
    * Break the message in the packet down for parsing.
    * @param msg the contents portion of the message, a space-separated `String`
    * @return the contents portion of the message, transformed into an `Array`
    */
  private def decomposeMessage(msg : String) : Array[String] = {
    msg.trim.toLowerCase.split("\\s+")
  }

  /**
    * Send a message back to the `Traveler` that will be printed into his chat window.
    * @param traveler the player
    * @param msg the message to be sent
    */
  private def reply(traveler : Traveler, msg : String) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", msg, None))
    )
  }

  /**
    * Print usage information to the `Traveler`'s chat window.<br>
    * <br>
    * The "official" use information for help dictates the command should follow this format:
    * `/warp &lt;x&gt;&lt;y&gt;&lt;z&gt; | to &lt;character&gt; | near &lt;object&gt; | above &lt;object&gt; | waypoint`.
    * In our case, creating fixed coordinate points of interest is not terribly dissimilar from the "near" and "to" aspect.
    * We can not currently implement most of the options for now, however.<br>
    * <br>
    * The destination prioritizes evaluation of the coordinates before the location string.
    * When the user provides coordinates, he must provide all three components of the coordinate at once, else none will be accepted.
    * If the coordinates are invalid, the location string will still be checked.
    * "-list" is accepted while no serious attempt is made to indicate a destination (no location string or not enough coordinates).
    * @param traveler the player
    */
  private def help(traveler : Traveler) : Unit = {
    CSRWarp.reply(traveler, "usage: /warp <location> | <gatename> | <x> <y> <z> | [-list]")
  }

  /**
    * Print error information to the `Traveler`'s chat window.<br>
    * The most common reason for error is the lack of information, or wrong information.
    * @param traveler the player
    */
  private def error(traveler : Traveler, msg : String) : Unit = {
    CSRWarp.reply(traveler, "Error! "+msg)
  }
}

/**
  * `Transfer` is a functional class intended to generalize the movement of a `Traveler`.<br>
  * <br>
  * Although a specific process for manually forcing a player avatar into a certain position surely exists, it is not currently known.
  * To sidestep this knowledge limitation, the player's avatar is deconstructed whenever it is moved.
  * It is then reconstructed in the place specified by the zone and the destination.
  * The process should be replaced (for warping, anyway) as soon as the formal methodology accepted by the client is understood.
  */
object Transfer {
  /**
    * This function manages player movement within the same zone, as specified by the `/warp` command.
    * @param traveler the player
    * @param destination a three-coordinate location in the zone
    */
  def warp(traveler : Traveler, destination : (Int, Int, Int)): Unit = {
    moveSelf(traveler, destination)
  }

  /**
    * This function manages player movement between zones, as specified by the `/zone` command.
    * @param traveler the player
    * @param zone the `Zone` requested
    * @param destination a three-coordinate location in the zone
    */
  def zone(traveler : Traveler, zone : Zone, destination : (Int, Int, Int)) : Unit = {
    disposeSelf(traveler)
    loadMap(traveler, zone)
    loadSelf(traveler, destination)
  }

  /**
    * The first step involved in moving the player (like we do) is to deconstruct the player in the client.
    * This operation is carried out through a series of `ObjectDeleteMessage` packets that undoes every item in the player's inventory.
    * The last operation undoes the player's avatar itself.<br>
    * <br>
    * This function uses static object GUIDs only because the `ObjectCreateMessage` data that generates the player is also static.
    * When that irons out, this simplistic step will no longer be valid, even as part of demonstration functionality.<br>
    * <br>
    * Sequential GUIDs that appear to be missing from the player's inventory have been noted.
    * @param traveler the player
    */
  private def disposeSelf(traveler : Traveler) : Unit = {
    //dispose inventory
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(76),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(77),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(78),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(79),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(80),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(81),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(82),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(83),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(84),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(85),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(86),4)))
//    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(87),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(88),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(89),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(90),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(91),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(92),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(93),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(94),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(95),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(96),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(97),4)))
    //dispose self
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(75),4)))
  }

  /**
    * Send the packet that causes the client to load a new `Zone`.<br>
    * <br>
    * The four latter parameters to `LoadMapMessage` certainly must do something; but, we do not care about them for now.
    * @param traveler the player
    * @param zone the `Zone` requested
    */
  def loadMap(traveler : Traveler, zone : Zone) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, LoadMapMessage(zone.map, zone.zonename, 40100,25,true,3770441820L)))
  }

  /**
    * Send the packets that create the avatar at a certain position in the current zone and assigns that avatar the status of "current."
    * The latter is necessary to be able to control and use that avatar on the given client.<br>
    * <br>
    * Disassemble the static `ObjectCreateMessage` data so that the existing coordinate data can be skipped over.
    * From the `loc`, the new coordinates for the position of the avatar are calculated and then pushed into the skipped space.
    * The resulting packet is sent to the client.
    * We have to convert all the way into a `BitVector` during the calculation process and the replace process.
    * Stopping at only `ByteVector` would result in the data chunks being padded inappropriately.
    * @param traveler the player
    * @param loc where the player is being placed in three dimensional space
    */
  def loadSelf(traveler : Traveler, loc : (Int, Int, Int)) : Unit = {
    //calculate bit representation of modified coordinates
    val pos : BitVector = Vector3.codec_pos.encode(Vector3(loc._1, loc._2, loc._3)).toOption.get.toByteVector.toBitVector
    //edit in modified coordinates
    val pkt = PlayerStateShiftMessage(ShiftState(0,Vector3(loc._1.toFloat, loc._2.toFloat, loc._3.toFloat), 0))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, pkt))

    var temp : BitVector = traveler.player.toBitVector
    temp = temp.take(68) ++ pos ++ temp.drop(124)
    //send
    traveler.sendToSelf(temp.toByteVector)
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(PlanetSideGUID(75),0,0)))

    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, BattleExperienceMessage(PlanetSideGUID(75),100000000,0)))

    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,1))) // Medium Assault
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,2))) // Heavy Assault
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,3))) // Special Assault
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,4))) // Anti-Vehicular
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,5))) // Sniping
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,6))) // Elite Assault
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,7))) // Air Cavalry, Scout
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,8))) // Air Cavalry, Interceptor
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,9))) // Air Cavalry, Assault
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,10))) // Air Support
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,11))) // ATV
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,12))) // Light Scout
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,13))) // Assault Buggy
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,14))) // Armored Assault 1
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,15))) // Armored Assault 2
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,16))) // Ground Transport
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,17))) // Ground Support
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,18))) // BattleFrame Robotics
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,19))) // Flail
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,20))) // Switchblade
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,21))) // Harasser
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,22))) // Phantasm
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,23))) // Galaxy Gunship
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,24))) // BFR Anti Aircraft
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,25))) // BFR Anti Infantry
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,28))) // Reinforced ExoSuit
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,29))) // Infiltration Suit
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,33))) // Uni-MAX
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,34))) // Medical
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,35))) // Advanced Medical
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,36))) // Hacking
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,37))) // Advanced Hacking
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,40))) // Electronics Expert
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,41))) // Engineering
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,42))) // Combat Engineering
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(75),24,45))) // Advanced Engineering
  }

  /**
    * Send the packet that moves the avatar to a certain position in the current zone.
    * @param traveler the player
    * @param loc where the player is being placed in three dimensional space
    */
  def moveSelf(traveler : Traveler, loc : (Int, Int, Int)) : Unit = {
    val pkt = PlayerStateShiftMessage(ShiftState(0,Vector3(loc._1.toFloat, loc._2.toFloat, loc._3.toFloat),0))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, pkt))
  }
}

/**
  * A crude representation of the information needed to describe a continent (hitherto, a "zone").
  * The information is mainly catered to the simulation of the CSR commands `/zone` and `/warp`.
  * (The exception is `alias` which is maintained for cosmetic purposes and clarification.)
  * @param alias the common name of the zone
  * @param map the map name of the zone (this map is loaded)
  * @param zonename the zone's internal name
  */
class Zone(val alias : String, val map : String, val zonename : String) {
  /**
    * A listing of warpgates, geowarps, and island warpgates in this zone.
    * The coordinates specified will only ever drop the user on a specific point within the protective bubble of the warpgate.
    * This breaks from the expected zoning functionality where the user is placed in a random spot under the bubble.
    * There is no prior usage details for the searchability format of this field's key values.
    */
  private val gates : mutable.HashMap[String, (Int, Int, Int)] = mutable.HashMap()
  /**
    * A listing of special locations in this zone, i.e., major faciities, and some landmarks of interest.
    * There is no prior usage details for the searchability format of this field's key values.
    */
  private val locations : mutable.HashMap[String, (Int, Int, Int)] = mutable.HashMap()
}

object Zone {
  /**
    * A listing of all zones that can be visited by their internal name.
    * The keys in this map should be directly usable by the `/zone` command.
    */
  private val zones = immutable.HashMap[String, Zone](
    "z1" -> Zone("Solsar", "map01", "z1"),
    "z2" -> Zone("Hossin", "map02", "z2"),
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
    "tzshtr" -> Zone("VR Shooting Range TR", "map14", "tzshtr"),
    "tzdrtr" -> Zone("VR Driving Range TR","map15", "tzdrtr"),
    "tzcotr" -> Zone("VR Combat Zone TR","map16", "tzcotr"),
    "tzshvs" -> Zone("VR Shooting Range VS", "map14", "tzshvs"),
    "tzdrvs" -> Zone("VR Driving Range VS","map15", "tzdrvs"),
    "tzcovs" -> Zone("VR Combat Zone VS","map16", "tzcovs"),
    "tzshnc" -> Zone("VR Shooting Range NC", "map14", "tzshnc"),
    "tzdrnc" -> Zone("VR Driving Range NC","map15", "tzdrnc"),
    "tzconc" -> Zone("VR Combat Zone NC","map16", "tzconc"),
    "c1" -> Zone("Supai", "ugd01", "c1"),
    "c2" -> Zone("Hunhau", "ugd02", "c2"),
    "c3" -> Zone("Adlivun", "ugd03", "c3"),
    "c4" -> Zone("Byblos", "ugd04", "c4"),
    "c5" -> Zone("Annwn", "ugd05", "c5"),
    "c6" -> Zone("Drugaskan", "ugd06", "c6"),
    "i4" -> Zone("Nexus", "map96", "i4"),
    "i3" -> Zone("Desolation", "map97", "i3"),
    "i2" -> Zone("Ascension", "map98", "i2"),
    "i1" -> Zone("Extinction", "map99", "i1"),
    "homebo" -> Zone("Black_ops_hq","Black_ops_hq", "homebo"),
    "station1" -> Zone("TR Station","Station1", "station1"),
    "station2" -> Zone("NC Station","Station2", "station2"),
    "station3" -> Zone("VS Station","Station3", "station3")
  )

  /**
    * A listing of all zones that can be visited by their common name.
    * The keys in this map should be directly usable by the `/zone` command.
    * Though the behavior is undocumented, access to this alias list is for the benefit of the user.
    */
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
    "tr-shooting" -> "tzshtr",
    "tr-driving" -> "tzdrtr",
    "tr-combat" -> "tzcotr",
    "vs-shooting" -> "tzshvs",
    "vs-driving" -> "tzdrvs",
    "vs-combat" -> "tzcovs",
    "nc-shooting" -> "tzshnc",
    "nc-driving" -> "tzdrnc",
    "nc-combat" -> "tzconc",
    "supai" -> "c1",
    "hunhau" -> "c2",
    "adlivun" -> "c3",
    "byblos" -> "c4",
    "annwn" -> "c5",
    "drugaskan" -> "c6",
    "nexus" -> "i4",
    "desolation" -> "i3",
    "ascension" -> "i2",
    "extinction" -> "i1",
    "Black_ops_hq" -> "homebo",
    "TR-Station" -> "station1",
    "NC-Station" -> "station2",
    "VS-Station" -> "station3"
  )
  /**
    * A value used for selecting where to appear in a zone from the list of locations when the user has no indicated one.
    */
  private val rand = Random
  setup()

  /**
    * An abbreviated constructor for creating `Zone`s without invocation of `new`.
    * @param alias the common name of the zone
    * @param map the map name of the zone (this map is loaded)
    * @param zonename the zone's internal name
    */
  def apply(alias : String, map : String, zonename : String) : Zone = new Zone(alias, map, zonename)

  /**
    * Get a valid `Zone`'s information.
    * @param zoneId a name that describes the zone and should be searchable
    * @return the `Zone`, or `None`
    */
  def get(zoneId : String) : Option[Zone] = {
    var zId = zoneId.toLowerCase
    if(alias.get(zId).isDefined)
      zId = alias(zId)

    zones.get(zId)
  }

  /**
    * Get a location within the `Zone`.
    * The location should be a facility or a warpgate or interesting.
    * @param zone the `Zone`
    * @param locId a name that describes a known location in the provided `Zone` and is searchable
    * @return the coordinates of that location, or None
    */
  def getWarpLocation(zone : Zone, locId : String) : Option[(Int, Int, Int)] = {
    val low_locId = locId.toLowerCase
    var location = zone.locations.get(low_locId)
    if(location.isEmpty)
      location = zone.gates.get(low_locId)
    location
  }

  /**
    * Get the position of a warpgate within the zone.
    * @param zone the `Zone`
    * @param gateId a name that describes a known warpgate in the provided `Zone` and is searchable
    * @return the coordinates of that warpgate, or None
    */
  def getWarpgate(zone : Zone, gateId : String) : Option[(Int, Int, Int)] = {
    zone.gates.get(gateId.toLowerCase)
  }

  /**
    * Get the names for all of the `Zones` that can be visited.
    * @return all of the zonenames
    */
  def list : String = {
    "zonenames: z1 - z10, home1 - home3, tzshnc, tzdrnc, tzconc, tzshtr, tzdrtr, tzcotr, tzshvs, tzdrvs, tzcovs, c1 - c6, i1 - i4; zones are also aliased to their continent name"
  }

  /**
    * Get the name for all of the locations that can be visited in this `Zone`, excluding warpgates.
    * @param zone the `Zone`
    * @return all of the location keys
    */
  def listLocations(zone : Zone) : String = {
    var out : String = "warps: "
    if(zone.locations.nonEmpty) {
      out += zone.locations.keys.toArray.sorted.mkString(", ")
    }
    else
      out = "none"
    out
  }

  /**
    * Get the name for all of the warpgates that can be visited in this `Zone`.
    * @param zone the `Zone`
    * @return all of the warpgate keys
    */
  def listWarpgates(zone : Zone) : String = {
    var out : String = "gatenames: "
    if(zone.gates.isEmpty)
      out += "none"
    else
      out += zone.gates.keys.toArray.sorted.mkString(", ")
    out
  }

  /**
    * Select, of all the `Zone` locations and warpgates, a pseudorandom destination to spawn the player in the zone if none has been specified.
    * @param zone the `Zone`
    * @return the coordinates of the spawn point
    */
  def selectRandom(zone : Zone) : (Int, Int, Int) = {
    var outlets = zone.locations //random location?
    if(outlets.nonEmpty) {
      return outlets.values.toArray.apply(rand.nextInt(outlets.size))
    }
    outlets = zone.gates //random warpgate?
    if(outlets.nonEmpty) {
      return outlets.values.toArray.apply(rand.nextInt(outlets.size))
    }
    (0, 0, 0) //fallback coordinates (that will always be valid)
  }

  /**
    * Load all zones with selected places of interest and the coordinates to place the player nearby that given place of interest.
    * All of these keys should be searchable under the `/warp` command.
    * Only the warpgate keys are searchable by the `/zone` command.
    */
  def setup() : Unit = {
    zones("z1").gates += (
      "gate1" -> (4150, 7341, 82),
      "gate2" -> (5698, 3404, 129),
      "gate3" -> (2650, 5363, 176),
      "gate4" -> (3022, 1225, 66),
      "geowarp1" -> (3678, 2895, 108),
      "geowarp2" -> (5672, 4750, 70)
      )
    zones("z1").locations += (
      "amun" -> (4337, 2278, 68),
      "aton" -> (3772, 5463, 54),
      "bastet" -> (5412, 5588, 56),
      "hapi" -> (4256, 4436, 59),
      "horus" -> (3725, 2114, 73),
      "mont" -> (3354, 4205, 83),
      "seth" -> (4495, 6026, 58),
      "sobek" -> (3094, 3027, 75),
      "thoth" -> (4615, 3373, 53),
      "lake" -> (4317, 4008, 37),
      "monolith" -> (5551, 5047, 64)
      )
    zones("z2").gates += (
      "gate1" -> (1881, 4873, 19),
      "gate2" -> (4648, 4625, 28),
      "gate3" -> (3296, 2045, 21),
      "gate4" -> (5614, 1781, 32),
      "geowarp1" -> (5199, 4869, 39),
      "geowarp2" -> (3911, 2407, 15)
      )
    zones("z2").locations += (
      "acan" -> (3534, 4015, 30),
      "bitol" -> (4525, 2632, 30),
      "chac" -> (4111, 5950, 39),
      "ghanon" -> (2565, 3707, 41),
      "hurakan" -> (1840, 2934, 38),
      "ixtab" -> (3478, 3143, 40),
      "kisin" -> (3356, 5374, 31),
      "mulac" -> (5592, 2738, 37),
      "naum" -> (5390, 3454, 28),
      "voltan" -> (4529, 3414, 28),
      "zotz" -> (6677, 2342, 129),
      "monolith" -> (2938, 2485, 14)
      )
    zones("z3").gates += (
      "gate1" -> (2616, 6567, 58),
      "gate2" -> (6980, 5336, 57),
      "gate3" -> (1199, 1332, 66),
      "gate4" -> (5815, 1974, 63),
      "geowarp1" -> (2403, 4278, 60),
      "geowarp2" -> (4722, 2665, 78)
      )
    zones("z3").locations += (
      "aja" -> (754, 5435, 48),
      "chuku" -> (4208, 7021, 54),
      "bomazi" -> (1198, 4492, 58),
      "ekera" -> (5719, 6555, 51),
      "faro" -> (5030, 5700, 57),
      "gunuku" -> (4994, 4286, 54),
      "honsi" -> (4042, 4588, 89),
      "itan" -> (5175, 3393, 48),
      "kaang" -> (5813, 3862, 62),
      "leza" -> (2691, 1561, 64),
      "mukuru" -> (661, 2380, 54),
      "nzame" -> (1670, 2706, 45),
      "orisha" -> (7060, 1327, 59),
      "pamba" -> (7403, 3123, 63),
      "shango" -> (6846, 2319, 63),
      "tore" -> (3017, 2272, 58),
      "wele" -> (436, 7040, 60),
      "monolith" -> (4515, 4105, 38),
      "peak" -> (3215, 5063, 579)
      )
    zones("z4").gates += (
      "gate1" -> (4702, 6768, 30),
      "gate2" -> (5515, 3368, 69),
      "gate3" -> (1564, 3356, 46),
      "gate4" -> (3889, 1118, 56),
      "geowarp1" -> (4202, 4325, 68),
      "geowarp2" -> (2384, 1925, 37)
      )
    zones("z4").locations += (
      "akkan" -> (2746, 4260, 39),
      "baal" -> (825, 5470, 72),
      "dagon" -> (1739, 5681, 40),
      "enkidu" -> (3217, 3574, 37),
      "girru" -> (4475, 5853, 78),
      "hanish" -> (3794, 5540, 89),
      "irkall" -> (4742, 5270, 66),
      "kusag" -> (6532, 4692, 46),
      "lahar" -> (6965, 5306, 38),
      "marduk" -> (3059, 2144, 70),
      "neti" -> (3966, 2417, 80),
      "zaqar" -> (4796, 2177, 75),
      "monolith" -> (5165, 4083, 35),
      "stonehenge" -> (4992, 3776, 56)
      )
    zones("z5").gates += (
      "gate1" -> (3432, 6498, 73),
      "gate2" -> (7196, 3917, 47),
      "gate3" -> (1533, 3540, 56),
      "gate4" -> (3197, 1390, 45),
      "geowarp1" -> (4899, 5633, 38),
      "geowarp2" -> (5326, 2558, 54)
      )
    zones("z5").locations += (
      "anu" -> (3479, 2556, 56),
      "bel" -> (3665, 4626, 58),
      "caer" -> (4570, 2601, 56),
      "dagd" -> (5825, 4449, 55),
      "eadon" -> (2725, 2853, 53),
      "gwydion" -> (5566, 3739, 61),
      "lugh" -> (6083, 5069, 72),
      "neit" -> (4345, 4319, 76),
      "ogma" -> (3588, 3227, 114),
      "pwyll" -> (4683, 4764, 104),
      "monolith" -> (3251, 3245, 160),
      "islands1" -> (6680, 6217, 125),
      "islands2" -> (1059, 6213, 120)
      )
    zones("z6").gates += (
      "gate1" -> (5040, 4327, 46),
      "gate2" -> (2187, 5338, 30),
      "gate3" -> (4960, 1922, 15),
      "gate4" -> (2464, 3088, 189),
      "geowarp1" -> (3221, 5328, 242),
      "geowarp2" -> (2237, 1783, 238)
      )
    zones("z6").locations += (
      "akna" -> (4509, 3732, 219),
      "anguta" -> (3999, 4170, 266),
      "igaluk" -> (3241, 5658, 235),
      "keelut" -> (3630, 1904, 265),
      "nerrivik" -> (3522, 3703, 322),
      "pinga" -> (5938, 3545, 96),
      "sedna" -> (3932, 5160, 232),
      "tarqaq" -> (2980, 2155, 237),
      "tootega" -> (5171, 3251, 217),
      "monolith" -> (4011, 4851, 32),
      "bridge" -> (3729, 4859, 234)
      )
    zones("z7").gates += (
      "gate1" -> (1516, 6448, 61),
      "gate2" -> (5249, 3819, 69),
      "gate3" -> (2763, 2961, 86),
      "gate4" -> (6224, 1152, 78),
      "geowarp1" -> (6345, 4802, 90),
      "geowarp2" -> (3800, 2197, 64)
      )
    zones("z7").locations += (
      "andvari" -> (3233, 7207, 78),
      "dagur" -> (4026, 6191, 60),
      "eisa" -> (3456, 4513, 75),
      "freyr" -> (2853, 3840, 56),
      "gjallar" -> (1056, 2656, 74),
      "helheim" -> (5542, 2532, 53),
      "jarl" -> (1960, 5462, 68),
      "kvasir" -> (4096, 1571, 69),
      "mani" -> (5057, 4989, 58),
      "nott" -> (6783, 4329, 46),
      "ran" -> (2378, 1919, 85),
      "vidar" -> (3772, 3024, 67),
      "ymir" -> (1911, 4008, 69),
      "monolith" -> (6390, 1622, 63)
      )
    zones("z8").gates += (
      "gate1" -> (5437, 5272, 32),
      "gate2" -> (3251, 5650, 60),
      "gate3" -> (5112, 2616, 40),
      "gate4" -> (2666, 1665, 45),
      "geowarp1" -> (3979, 5370, 47),
      "geowarp2" -> (6018, 3136, 35)
      )
    zones("z8").locations += (
     "atar" -> (3609, 2730, 47),
      "dahaka" -> (4633, 5379, 54),
      "hvar" -> (3857, 4764, 49),
      "izha" -> (5396, 3852, 51),
      "jamshid" -> (2371, 3378, 52),
      "mithra" -> (2480, 4456, 44),
      "rashnu" -> (3098, 3961, 59),
      "yazata" -> (4620, 3983, 62),
      "zal" -> (3966, 2164, 61),
      "arch1" -> (4152, 3285, 31),
      "arch2" -> (4688, 5272, 68),
      "pride" -> (2913, 4412, 63)
      )
    zones("z9").gates += (
      "gate1" -> (1505, 6981, 65),
      "gate2" -> (6835, 3517, 56),
      "gate3" -> (1393, 1376, 53),
      "geowarp1" -> (7081, 5552, 46),
      "geowarp2" -> (3776, 1092, 49)
      )
    zones("z9").locations += (
      "akua" -> (5258, 4041, 346),
      "drakulu" -> (3806, 2647, 151),
      "hiro" -> (4618, 5761, 190),
      "iva" -> (6387, 5199, 55),
      "karihi" -> (3879, 5574, 236),
      "laka" -> (4720, 6718, 49),
      "matagi" -> (5308, 5093, 239),
      "ngaru" -> (4103, 4077, 205),
      "oro" -> (4849, 4456, 208),
      "pele" -> (4549, 3712, 208),
      "rehua" -> (3843, 2195, 60),
      "sina" -> (5919, 2177, 91),
      "tara" -> (1082, 4225, 60),
      "wakea" -> (1785, 5241, 63),
      "monolith" -> (3246, 6507, 105)
      )
    zones("z10").gates += (
      "gate1" -> (6140, 6599, 71),
      "gate2" -> (4814, 4608, 59),
      "gate3" -> (3152, 3480, 54),
      "gate4" -> (1605, 1446, 40),
      "geowarp1" -> (3612, 6918, 38),
      "geowarp2" -> (3668, 3327, 55)
      )
    zones("z10").locations += (
      "azeban" -> (6316, 5160, 62),
      "cetan" -> (3587, 2522, 48),
      "heyoka" -> (4395, 2327, 47),
      "ikanam" -> (2740, 2412, 57),
      "kyoi" -> (5491, 2284, 62),
      "mekala" -> (6087, 2925, 59),
      "onatha" -> (3397, 5799, 48),
      "qumu" -> (3990, 5152, 46),
      "sungrey" -> (4609, 5624, 72),
      "tumas" -> (4687, 6392, 69),
      "verica" -> (4973, 3459, 47),
      "xelas" -> (6609, 4479, 56),
      "monolith" -> (5651, 6024, 38)
      )
    zones("home1").gates += (
      "gate1" -> (4158, 6344, 44),
      "gate2" -> (2214, 5797, 48),
      "gate3" -> (5032, 3241, 53)
      )
    zones("home1").locations += "hart_c" -> (2352, 5523, 66)
    zones("home2").gates += (
      "gate1" -> (5283, 4317, 44),
      "gate2" -> (3139, 4809, 40),
      "gate3" -> (3659, 2894, 26)
      )
    zones("home2").locations += "hart_c" -> (3126, 2864, 35)
    zones("home3").gates += (
      "gate1" -> (5657, 4681, 98),
      "gate2" -> (2639, 5366, 57),
      "gate3" -> (4079, 2467, 155)
      )
    zones("home3").locations += "hart_c" -> (3675, 2727, 91)
    zones("tzshtr").locations += "roof" -> (499, 1568, 25)
    zones("tzcotr").locations += "spawn" -> (960, 1002, 32)
    zones("tzdrtr").locations += (
      "start" -> (2457, 1864, 23),
      "air_pad" -> (1700, 1900, 32)
      )
    zones("tzshvs").locations += "roof" -> (499, 1568, 25)
    zones("tzcovs").locations += "spawn" -> (960, 1002, 32)
    zones("tzdrvs").locations += (
      "start" -> (2457, 1864, 23),
      "air_pad" -> (1700, 1900, 32)
      )
    zones("tzshnc").locations += "roof" -> (499, 1568, 25)
    zones("tzconc").locations += "spawn" -> (960, 1002, 32)
    zones("tzdrnc").locations += (
      "start" -> (2457, 1864, 23),
      "air_pad" -> (1700, 1900, 32)
      )
    zones("c1").gates += (
      "geowarp1" -> (998, 2038, 103),
      "geowarp2" -> (231, 1026, 82),
      "geowarp3" -> (2071, 1405, 102),
      "geowarp4" -> (1051, 370, 103)
      )
    zones("c2").gates += (
      "geowarp1" -> (999, 2386, 243),
      "geowarp2" -> (283, 1249, 172),
      "geowarp3" -> (1887, 1307, 192),
      "geowarp4" -> (1039, 155, 143)
      )
    zones("c3").gates += (
      "geowarp1" -> (1095, 1725, 25),
      "geowarp2" -> (226, 832, 42),
      "geowarp3" -> (1832, 1026, 43),
      "geowarp4" -> (981, 320, 46)
      )
    zones("c4").gates += (
      "geowarp1" -> (902, 1811, 93),
      "geowarp2" -> (185, 922, 113),
      "geowarp3" -> (1696, 1188, 92),
      "geowarp4" -> (887, 227, 115)
      )
    zones("c5").gates += (
      "geowarp1" -> (1195, 1752, 244),
      "geowarp2" -> (290, 1104, 235),
      "geowarp3" -> (1803, 899, 243),
      "geowarp4" -> (1042, 225, 246)
      )
    zones("c6").gates += (
      "geowarp1" -> (1067, 2044, 95),
      "geowarp2" -> (290, 693, 73),
      "geowarp3" -> (1922, 928, 33),
      "geowarp4" -> (1174, 249, 114)
      )
    zones("i3").gates += (
      "gate1" -> (1219, 2580, 30),
      "gate2" -> (2889, 2919, 33),
      "gate3" -> (2886, 1235, 32)
      )
    zones("i3").locations += (
      "dahaka" -> (1421, 2216, 30),
      "jamshid" -> (2500, 2543, 30),
      "izha" -> (2569, 1544, 30),
      "oasis" -> (2084, 1935, 40)
      )
    zones("i2").gates += (
      "gate1" -> (1243, 1393, 12),
      "gate2" -> (2510, 2544, 12),
      "gate3" -> (2634, 1477, 12)
      )
    zones("i2").locations += (
      "rashnu" -> (1709, 1802, 91),
      "sraosha" -> (2729, 2349, 91),
      "zal" -> (1888, 2728, 91),
      "center" -> (2082, 2192, 160)
      )
    zones("i1").gates += (
      "gate1" -> (1225, 2036, 67),
      "gate2" -> (2548, 2801, 65),
      "gate3" -> (2481, 1194, 89)
      )
    zones("i1").locations += (
      "hvar" -> (1559, 1268, 88),
      "mithra" -> (2855, 2850, 89),
      "yazata" -> (1254, 2583, 88),
      "south_of_volcano" -> (2068, 1686, 88)
      )
    zones("i4").gates += (
      "gate1" -> (2359, 2717, 36),
      "gate2" -> (2732, 1355, 36),
      "geowarp" -> (1424, 1640, 45)
      )
    zones("i4").locations += "atar" -> (1981, 1886, 36)
  }
}
