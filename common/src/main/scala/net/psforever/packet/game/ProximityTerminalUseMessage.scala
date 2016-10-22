// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * The player's avatar has moved in relation to a set piece that interacts with the player due to his proximity to it.<br>
  * <br>
  * Elements of this nature include Rearm/Repair Silos in courtyards and various cavern crystals.
  * The packets are only dispatched when it is appropriate for the player to be affected.<br>
  * <br>
  * Exploration:<br>
  * Packets where the bytes for the player's GUID are blank exist.
  * @param player_guid the player
  * @param object_guid the object whose functionality is triggered by the player's distance to it
  * @param unk unk
  */
final case class ProximityTerminalUseMessage(player_guid : PlanetSideGUID,
                                             object_guid : PlanetSideGUID,
                                             unk : Boolean)
  extends PlanetSideGamePacket {
  type Packet = ProximityTerminalUseMessage
  def opcode = GamePacketOpcode.ProximityTerminalUseMessage
  def encode = ProximityTerminalUseMessage.encode(this)
}

object ProximityTerminalUseMessage extends Marshallable[ProximityTerminalUseMessage] {
  implicit val codec : Codec[ProximityTerminalUseMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("object_guid" | PlanetSideGUID.codec) ::
      ("unk" | bool)
    ).as[ProximityTerminalUseMessage]
}
