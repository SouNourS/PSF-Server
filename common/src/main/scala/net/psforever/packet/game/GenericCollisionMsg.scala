// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

/*
[ INFO] WorldSessionActor - GenericCollisionMsg: ByteVector(59 bytes, 0x92c00000
1900000020c9a0efd71f730fe2d1e7f040000000000000000000000000725f84716b058000000000
0000000000000000000023a03a1140)
 */
import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

final case class GenericCollisionMsg(unk : ByteVector)
  extends PlanetSideGamePacket {
  type Packet = GenericCollisionMsg
  def opcode = GamePacketOpcode.GenericCollisionMsg
  def encode = GenericCollisionMsg.encode(this)
}

object GenericCollisionMsg extends Marshallable[GenericCollisionMsg] {
  implicit val codec : Codec[GenericCollisionMsg] = (
    ("unk" | bytes)
    ).as[GenericCollisionMsg]
}
