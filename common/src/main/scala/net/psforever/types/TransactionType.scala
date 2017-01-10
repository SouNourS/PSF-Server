// Copyright (c) 2016 PSForever.net to present
package net.psforever.types

import net.psforever.packet.PacketHelpers
import scodec.codecs._

object TransactionType extends Enumeration {
  type Type = Value
  val Unk0,
      Learn, // certif term
      Buy,
      Sell, // forget on certif term
      Unk4,
      Unk5,
      Unk6,
      Unk7
      = Value

  implicit val codec = PacketHelpers.createEnumerationCodec(this, uintL(3))
}
