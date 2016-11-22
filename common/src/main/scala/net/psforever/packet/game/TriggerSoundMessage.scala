// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

//6b 18 e6 65 70 2e 20 00 80 f1 fc
//0001 1000 1110 0110 0110 0101 0111 0000 0010 1110 0010 0000 0000 0000 1000 0000 1111 0001 1111 1100
//xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx
//aaaa aaaa aaaa aaaa ---- ---b bbbb bbbb bbbb bccc cccc cccc cccd dddd dddd deee eeee eeff ffff ff--
/*
6b 02d6ab 89ced0 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab 89ced0 b07192c8
6b 02d6ab 89ced0 b07192c8
6b 02d6ab 89ced0 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab 89ced0 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
6b 02d6ab efca50 b07192c8
 */
final case class TriggerSoundMessage(player_guid : PlanetSideGUID,
                                     unk1 : Int,
                                     pos : Vector3,
                                     unk2 : Int,
                                     unk3 : Int)
  extends PlanetSideGamePacket {
  type Packet = TimeOfDayMessage
  def opcode = GamePacketOpcode.TriggerSoundMessage
  def encode = TriggerSoundMessage.encode(this)
}

object TriggerSoundMessage extends Marshallable[TriggerSoundMessage] {
  implicit val codec : Codec[TriggerSoundMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("unk1" | uintL(7)) ::
      ("pos" | Vector3.codec_pos) ::
      ("unk2" | uintL(9)) ::
      ("unk3" | uint8L)
    ).as[TriggerSoundMessage]
}
