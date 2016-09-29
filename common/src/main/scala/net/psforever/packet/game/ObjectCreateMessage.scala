package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.bits._
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless._

case class Ammunition(bit_length : Long,
                      unk1 : Boolean, //used in determining parentage
                      target_guid : PlanetSideGUID,
                      item_type : Int,
                      item_guid : PlanetSideGUID,
                      target_index : Int,
                      unk2 : Int,
                      capacity : Int,
                      unk3 : Int)

object Ammunition extends Marshallable[Ammunition] {
  val codec : Codec[Ammunition] = (
    ("bit_length" | uint32L) ::
      ("unk1" | bool) ::
      ("target_guid" | PlanetSideGUID.codec) ::
      ("item_type" | uintL(11)) ::
      ("item_guid" | PlanetSideGUID.codec) ::
      ("target_index" | uint8L) ::
      ("unk2" | uintL(23)) ::
      ("capacity" | uint8L) ::
      ("unk3" | uintL(13))
    ).as[Ammunition]
}

case class ObjectCreateMessageParent(guid : Int, slot : Int)

case class ObjectCreateMessage(streamLength : Long, // in bits
                               objectClass : Int,
                               guid : Int,
                               parentInfo : Option[ObjectCreateMessageParent],
                               stream : BitVector
                              )
  extends PlanetSideGamePacket {

  def opcode = GamePacketOpcode.ObjectCreateMessage
  def encode = ObjectCreateMessage.encode(this)
}

object ObjectCreateMessage extends Marshallable[ObjectCreateMessage] {

  type Pattern = Int :: Int :: Option[ObjectCreateMessageParent] :: HNil
  type ChoicePattern = Either[Pattern, Pattern]

  val noParent : Codec[Pattern] = (("object_class" | uintL(0xb)) ::
    ("guid" | uint16L)).xmap[Pattern]( {
    case cls :: guid :: HNil => cls :: guid :: None :: HNil
  }, {
    case cls :: guid :: None :: HNil => cls :: guid :: HNil
  })
  val parent : Codec[Pattern] = (("parent_guid" | uint16L) ::
    ("object_class" | uintL(0xb)) ::
    ("guid" | uint16L) ::
    ("parent_slot_index" | PacketHelpers.encodedStringSize)).xmap[Pattern]( {
    case pguid :: cls :: guid :: slot :: HNil =>
      cls :: guid :: Some(ObjectCreateMessageParent(pguid, slot)) :: HNil
  }, {
    case cls :: guid :: Some(ObjectCreateMessageParent(pguid, slot)) :: HNil =>
      pguid :: cls :: guid :: slot :: HNil
  })

  implicit val codec : Codec[ObjectCreateMessage] = (
    ("stream_length" | uint32L) :: (either(bool, parent, noParent).exmap[Pattern]( {
      case Left(a :: b :: Some(c) :: HNil) => Attempt.successful(a :: b :: Some(c) :: HNil)
      case Right(a :: b :: None :: HNil) => Attempt.successful(a :: b :: None :: HNil)
        // failure cases
      case Left(a :: b :: None :: HNil) => Attempt.failure(Err("expected parent structure"))
      case Right(a :: b :: Some(c) :: HNil) => Attempt.failure(Err("got unexpected parent structure"))
    }, {
      case a :: b :: Some(c) :: HNil => Attempt.successful(Left(a :: b :: Some(c) :: HNil))
      case a :: b :: None :: HNil => Attempt.successful(Right(a :: b :: None :: HNil))
    }) :+ ("rest" | bits) )
    ).as[ObjectCreateMessage]
}
