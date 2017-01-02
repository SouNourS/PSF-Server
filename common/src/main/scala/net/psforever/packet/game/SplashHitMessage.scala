// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

final case class SplashHitMessage(bytes : ByteVector)
  extends PlanetSideGamePacket {
  type Packet = SplashHitMessage
  def opcode = GamePacketOpcode.SplashHitMessage
  def encode = SplashHitMessage.encode(this)
}

object SplashHitMessage extends Marshallable[SplashHitMessage] {
  implicit val codec : Codec[SplashHitMessage] = (
    ("bytes" | bytes)
    ).as[SplashHitMessage]
}
