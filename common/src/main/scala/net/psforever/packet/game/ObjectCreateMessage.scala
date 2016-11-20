package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.bits._
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless._

case class ObjectCreateMessageParent(guid : PlanetSideGUID,
                                     slot : Int)

case class ObjectCreateMessage(streamLength : Long, // in bits
                               objectClass : Int,
                               guid : PlanetSideGUID,
                               parentInfo : Option[ObjectCreateMessageParent],
                               stream : BitVector
                              )
  extends PlanetSideGamePacket {

  def opcode = GamePacketOpcode.ObjectCreateMessage
  def encode = ObjectCreateMessage.encode(this)
}

object ObjectCreateMessageParent extends Marshallable[ObjectCreateMessageParent] {
  implicit val codec : Codec[ObjectCreateMessageParent] = (
    ("guid" | uint8L) ::
      ("slot" | uint8L)
    ).as[ObjectCreateMessageParent]
}

private object ObjectInit {
  case class _ObjectInit(object_class : Int,
                                 object_guid : PlanetSideGUID,
                                 parent_info : Option[ObjectCreateMessageParent])

  def apply(data : ObjectCreateMessage.parentPattern) : _ObjectInit = data.asInstanceOf[_ObjectInit]

  def apply(x : Int, y : PlanetSideGUID, z : Option[ObjectCreateMessageParent]) : ObjectCreateMessage.parentPattern = x :: y :: z :: HNil
}

object ObjectCreateMessage extends Marshallable[ObjectCreateMessage] {
  type parentPattern = Int :: PlanetSideGUID :: Option[ObjectCreateMessageParent] :: HNil
  val parentCodec : Codec[parentPattern] = (
    ("parent_guid" | PlanetSideGUID.codec) ::
      ("object_class" | uintL(0xb)) ::
      ("guid" | PlanetSideGUID.codec) ::
      ("parent_slot_index" | uint8L)
    ).xmap[parentPattern] (
    {
      case pguid :: cls :: guid :: slot :: HNil =>
        cls :: guid :: Some(ObjectCreateMessageParent(pguid, slot)) :: HNil
    },
    {
      case cls :: guid :: Some(ObjectCreateMessageParent(pguid, slot)) :: HNil =>
        pguid :: cls :: guid :: slot :: HNil
    }
  )

  val noParentCodec : Codec[parentPattern] = (
    ("object_class" | uintL(0xb)) ::
      ("guid" | PlanetSideGUID.codec)
    ).xmap[parentPattern] (
    {
      case cls :: guid :: HNil =>
        cls :: guid :: None :: HNil
    },
    {
      case cls :: guid :: None :: HNil =>
        cls :: guid :: HNil
    }
  )

  implicit val info_codec : Codec[parentPattern] = (
    bool >>:~ { test =>
      newcodecs.binary_choice(test, parentCodec, noParentCodec)
    }
    ).exmap[parentPattern] (
    {
      case true :: cls :: guid :: Some(parent_info) :: HNil =>
        Attempt.successful(cls :: guid :: Some(parent_info) :: HNil)
      case false :: cls :: guid :: None :: HNil =>
        Attempt.successful(cls :: guid :: None :: HNil)
      case true :: cls :: guid :: None :: HNil =>
        Attempt.failure(Err("missing parent structure"))
      case false :: cls :: guid :: Some(parent_info) :: HNil =>
        Attempt.failure(Err("unexpected parent structure"))
    },
    {
      case cls :: guid :: Some(parent_info) :: HNil =>
        Attempt.successful(true :: cls :: guid :: Some(parent_info) :: HNil)
      case cls :: guid :: None :: HNil =>
        Attempt.successful(false :: cls :: guid :: None :: HNil)
    }
  )

  //TODO Int :: PlanetSideGUID :: Option[ObjectCreateMessageParent] --> parentPattern, but it can not be applied here, why not?
  type streamLengthPattern = Int :: PlanetSideGUID :: Option[ObjectCreateMessageParent] :: BitVector :: HNil
  val streamLengthCodec : Codec[streamLengthPattern] = (
    ("stream_length" | uint32L) ::
      info_codec ::
      ("stream" | bits)
    ).xmap[streamLengthPattern] (
    {
      case _ :: info :: data :: HNil =>
        ObjectInit(info).object_class :: ObjectInit(info).object_guid :: ObjectInit(info).parent_info :: data :: HNil
    },
    {
      case cls :: guid :: parent_info :: data :: HNil =>
        (32 + (if(parent_info.isDefined) 52 else 28) + data.size) :: ObjectInit(cls, guid, parent_info) :: data :: HNil
    }
  )

  type Pattern = Int :: PlanetSideGUID :: Option[ObjectCreateMessageParent] :: HNil

  val noParent : Codec[Pattern] = (("object_class" | uintL(0xb)) ::
    ("guid" | PlanetSideGUID.codec)).xmap[Pattern]( {
    case cls :: guid :: HNil => cls :: guid :: None :: HNil
  }, {
    case cls :: guid :: None :: HNil => cls :: guid :: HNil
  })
  val parent : Codec[Pattern] = (("parent_guid" | PlanetSideGUID.codec) ::
    ("object_class" | uintL(0xb)) ::
    ("guid" | PlanetSideGUID.codec) ::
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
