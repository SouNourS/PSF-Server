// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * The player is interacting with a zipline.
  * @param player_guid the player
  * @param origin_side whether this corresponds with the "entry" or the "exit" of the zipline, as per the direction of the light pulse visuals
  * @param action na; usually 0?
  * @param id na; a number that is consistent to a zipline point but is not a building GUID?
  * @param unk1 na; changes as the user moves; seems to be related to x or y coordinate of contact
  * @param unk2 na; changes as the user moves; seems to be related to y or x coordinate of contact
  * @param unk3 na; changes as the user moves; seems to be related to z (vertical) coordinate of contact
  */
final case class ZipLineMessage(player_guid : PlanetSideGUID,
                                origin_side : Boolean,
                                action : Int,
                                id : Long,
                                unk1 : Long,
                                unk2 : Long,
                                unk3 : Long)
  extends PlanetSideGamePacket {
  type Packet = ZipLineMessage
  def opcode = GamePacketOpcode.ZipLineMessage
  def encode = ZipLineMessage.encode(this)
}

object ZipLineMessage extends Marshallable[ZipLineMessage] {
  implicit val codec : Codec[ZipLineMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("origin_side" | bool) ::
      ("action" | uint2) ::
      ("id" | uint32L) ::
      ("unk3" | uint32L) ::
      ("unk4" | uint32L) ::
      ("unk5" | uint32L)
    ).as[ZipLineMessage]
}
