// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  *
  */
final case class LootItemMessage(unk1 : PlanetSideGUID,
                                       unk2 : PlanetSideGUID)
  extends PlanetSideGamePacket {
  type Packet = LootItemMessage
  def opcode = GamePacketOpcode.LootItemMessage
  def encode = LootItemMessage.encode(this)
}

object LootItemMessage extends Marshallable[LootItemMessage] {
  implicit val codec : Codec[LootItemMessage] = (
    ("unk1" | PlanetSideGUID.codec) ::
      ("unk2" | PlanetSideGUID.codec)
    ).as[LootItemMessage]
}
