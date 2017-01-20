// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.TransactionType
import scodec.Codec
import scodec.codecs._

final case class ItemTransactionResultMessage(terminal_guid : PlanetSideGUID,
                                              transaction_type : TransactionType.Value,
                                              success : Boolean,
                                              error_code : Int)
  extends PlanetSideGamePacket {
  type Packet = ItemTransactionResultMessage
  def opcode = GamePacketOpcode.ItemTransactionResultMessage
  def encode = ItemTransactionResultMessage.encode(this)
}

object ItemTransactionResultMessage extends Marshallable[ItemTransactionResultMessage] {
  implicit val codec : Codec[ItemTransactionResultMessage] = (
      ("terminal_guid" | PlanetSideGUID.codec) ::
        ("transaction_type" | TransactionType.codec) ::
        ("success" | bool) ::
        ("error_code" | uint8L)
    ).as[ItemTransactionResultMessage]
}