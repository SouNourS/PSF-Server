// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * Display a message in the event window that informs of a player death.<br>
  * <br>
  * The message is composed of three parts:<br>
  * 1) killer information<br>
  * 2) method information<br>
  * 3) victim information<br>
  * In the case of a player kill, the player's name will be attributed directly.
  * In the case of an absentee kill, a description of the method of death will be attributed.
  * In the case of a suicide, the player attributed is the player who was killed (message format displays only the victim).
  * When marked as being in a vehicle or turret, that player's name will be enclosed within square brackets.<br>
  * <br>
  * The fields that follow each name appears to be important to the identification of the associated player.
  * The same value will be seen in every `DestroyDisplayMessage` that includes the player.
  * This holds true for every entry within the same login session, at least.
  * Blanking these values out does not change anything about the format of the event message.
  * In the case of absentee kills, for example, where there is no killer listed, this field has been zero'd.<br>
  * <br>
  * The fields in between the killer section and the victim section are the method of homicide or suicide.
  * The color of the resulting icon is borrowed from the attributed killer's faction affiliation if it can be determined.
  * An unidentified method defaults to a skull and crossbones icon.<br>
  * <br>
  * Empire:<br>
  * 0 - Terran Republic<br>
  * 1 - New Conglomerate<br>
  * 2 - Vanu Sovereignty<br>
  * 3 - Neutral / Black OPs<br>
  * <br>
  * Exploration:<br>
  * How many combinations of method fields do not resolve in skull and crossbones?
  * @param killer the name of the player who did the killing
  * @param killer_unk See above
  * @param killer_empire the empire affiliation of the killer
  * @param killer_inVehicle `true`, if the killer was in a vehicle at the time of the kill
  * @param unk na;
  *            does not like being set to 0
  * @param method modifies the icon in the message, related to the way the victim was killed
  * @param victim the name of the player who was killed
  * @param victim_unk See above
  * @param victim_empire the empire affiliation of the victim
  * @param victim_inVehicle `true`, if the victim was in a vehicle when he was killed
  */
final case class DestroyDisplayMessage(killer : String,
                                       killer_unk : Long,
                                       killer_empire : Int,
                                       killer_inVehicle : Boolean,
                                       unk : PlanetSideGUID,
                                       method : PlanetSideGUID,
                                       victim : String,
                                       victim_unk : Long,
                                       victim_empire : Int,
                                       victim_inVehicle : Boolean
                                      )
  extends PlanetSideGamePacket {
  type Packet = DestroyDisplayMessage
  def opcode = GamePacketOpcode.DestroyDisplayMessage
  def encode = DestroyDisplayMessage.encode(this)
}

object DestroyDisplayMessage extends Marshallable[DestroyDisplayMessage] {
  implicit val codec : Codec[DestroyDisplayMessage] = (
    ("killer" | PacketHelpers.encodedWideString) ::
      ("killer_unk" | uint32L) ::
      ("killer_empire" | uint2L) ::
      ("killer_inVehicle" | bool) ::
      ("unk" | PlanetSideGUID.codec) ::
      ("method" | PlanetSideGUID.codec) ::
      ("victim" | PacketHelpers.encodedWideStringAligned(5)) ::
      ("victim_unk" | uint32L) ::
      ("victim_empire" | uint2L) ::
      ("victim_inVehicle" | bool)
    ).as[DestroyDisplayMessage]
}