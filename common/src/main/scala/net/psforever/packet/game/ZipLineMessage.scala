// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * The player is interacting with a zipline.<br>
  * <br>
  * Action:
  * `0 - Attach to zipline`
  * `1 - Arrived at destination`
  * `2 - Forcibly detach from zipline in mid-transit`
  * @param player_guid the player
  * @param origin_side whether this corresponds with the "entry" or the "exit" of the zipline, as per the direction of the light pulse visuals
  * @param action how the player interacts with the zipline
  * @param guid a number that is consistent to a zipline terminus
  * @param unk3 na;
  *             changes as the user moves;
  *             related to x or y coordinate of contact
  * @param unk4 na;
  *             changes as the user moves;
  *             related to y or x coordinate of contact
  * @param unk5 na;
  *             changes as the user moves;
  *             related to z (vertical) coordinate of contact
  */
final case class ZipLineMessage(player_guid : PlanetSideGUID,
                                origin_side : Boolean,
                                action : Int,
                                guid : Long,
                                unk3 : Long,
                                unk4 : Long,
                                unk5 : Long)
  extends PlanetSideGamePacket {
  type Packet = ZipLineMessage
  def opcode = GamePacketOpcode.ZipLineMessage
  def encode = ZipLineMessage.encode(this)
}

object ZipLineMessage extends Marshallable[ZipLineMessage] {
  implicit val codec : Codec[ZipLineMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("origin_side" | bool) ::
      ("action" | uint2L) ::
      ("guid" | uint32L) ::
      ("unk3" | uint32L) ::
      ("unk4" | uint32L) ::
      ("unk5" | uint32L)
    ).as[ZipLineMessage]
}
