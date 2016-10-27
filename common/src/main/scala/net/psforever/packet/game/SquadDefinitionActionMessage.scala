// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

final case class SquadDefinitionActionMessage(bytes : ByteVector)
  extends PlanetSideGamePacket {
  type Packet = SquadDefinitionActionMessage
  def opcode = GamePacketOpcode.SquadDefinitionActionMessage
  def encode = SquadDefinitionActionMessage.encode(this)
}

object SquadDefinitionActionMessage extends Marshallable[SquadDefinitionActionMessage] {
  implicit val codec : Codec[SquadDefinitionActionMessage] = (
    ("bytes" | bytes)
    ).as[SquadDefinitionActionMessage]
}
