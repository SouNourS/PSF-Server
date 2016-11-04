// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import scodec.codecs._
import shapeless._

//this packet is limited mainly by byte-size

//continents are stored in this packet as 32-bit numbers instead of 16-bit; after the normal 16 bits, two bytes can be ignored

final case class SquadInfo(leader : Option[String],
                           task : Option[String],
                           continent_guid : Option[PlanetSideGUID],
                           size : Option[Int],
                           capacity : Option[Int],
                           squad_guid : Option[PlanetSideGUID] = None)

final case class SquadHeader(action : Int,
                             unk : Boolean,
                             //action2 : Int,
                             info : SquadInfo)

final case class SquadListing(index : Int = 255,
                              listing : Option[SquadHeader] = None,
                              na : Option[BitVector] = None)

final case class ReplicationStreamMessage(behavior : Int,
                                          init : Option[ReplicationStreamMessage] = None,
                                          unk : Option[Boolean] = None,
                                          entries : Vector[SquadListing] = Vector.empty)
  extends PlanetSideGamePacket {
  type Packet = ReplicationStreamMessage
  def opcode = GamePacketOpcode.ReplicationStreamMessage
  def encode = ReplicationStreamMessage.encode(this)
}

object SquadInfo {
  //use: SquadInfo(leader, task, continent_guid, size, capacity)
  def apply(leader : String, task : String, continent_guid : PlanetSideGUID, size : Int, capacity : Int) : SquadInfo = {
    SquadInfo(Some(leader), Some(task), Some(continent_guid), Some(size), Some(capacity))
  }

  //use: SquadInfo(leader, task, continent_guid, size, capacity)
  def apply(leader : String, task : String, continent_guid : PlanetSideGUID, size : Int, capacity : Int, sguid : PlanetSideGUID) : SquadInfo = {
    SquadInfo(Some(leader), Some(task), Some(continent_guid), Some(size), Some(capacity), Some(sguid))
  }

  //use: SquadInfo(leader, None)
  def apply(leader : Option[String], task : String) : SquadInfo = {
    SquadInfo(leader, Some(task), None, None, None)
  }

  //use: SquadInfo(None, task)
  def apply(leader : String, task : Option[String]) : SquadInfo = {
    SquadInfo(Some(leader), task, None, None, None)
  }

  //use: SquadInfo(continent_guid)
  def apply(continent_guid : PlanetSideGUID) : SquadInfo = {
    SquadInfo(None, None, Some(continent_guid), None, None)
  }

  //use: SquadInfo(size, None)
  //we currently do not know the action codes that adjust squad capacity
  def apply(size : Int, capacity : Option[Int]) : SquadInfo = {
    SquadInfo(None, None, None, Some(size), None)
  }

  //use: SquadInfo(None, capacity)
  //we currently do not know the action codes that adjust squad capacity
  def apply(size : Option[Int], capacity : Int) : SquadInfo = {
    SquadInfo(None, None, None, size, Some(capacity))
  }
}

object SquadHeader extends Marshallable[SquadHeader] {
  type squadPattern = SquadInfo :: HNil
  val initCodec : Codec[squadPattern] = (
      ("squad_guid" | PlanetSideGUID.codec) ::
        ("leader" | PacketHelpers.encodedWideString) ::
        ("task" | PacketHelpers.encodedWideString) ::
        ("continent_guid" | PlanetSideGUID.codec) ::
        uint16L ::
        ("size" | uint4L) ::
        ("capacity" | uint4L)
    ).xmap[squadPattern](
    {
      case sguid :: lead :: tsk :: cguid :: x :: sz :: cap :: HNil =>
        SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil
    },
    {
      case SquadInfo(lead, task, cguid, sz, cap, sguid) :: HNil =>
        sguid.get :: lead.get :: task.get :: cguid.get :: 0 :: sz.get :: cap.get :: HNil
    }
  )
  val alt_initCodec : Codec[squadPattern] = (
    ("squad_guid" | PlanetSideGUID.codec) ::
      ("leader" | PacketHelpers.encodedWideStringAligned(7)) ::
      ("task" | PacketHelpers.encodedWideString) ::
      ("continent_guid" | PlanetSideGUID.codec) ::
      uint16L ::
      ("size" | uint4L) ::
      ("capacity" | uint4L)
    ).xmap[squadPattern](
      {
        case sguid :: lead :: tsk :: cguid :: x :: sz :: cap :: HNil =>
          SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil
      },
      {
        case SquadInfo(lead, task, cguid, sz, cap, sguid) :: HNil =>
          sguid.get :: lead.get :: task.get :: cguid.get :: 0 :: sz.get :: cap.get :: HNil
      }
    )

  val sizeCodec : Codec[squadPattern] = (
    uintL(3) ::
      bool ::
      uint4L
  ).xmap[squadPattern] (
    {
      case unk1 :: unk2 :: sz :: HNil =>
        SquadInfo(sz, None) :: HNil
    },
    {
      case SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil =>
        2 :: false :: sz.get :: HNil
    }
  )

  val continentCodec : Codec[squadPattern] = (
    uintL(3) ::
      bool ::
      PlanetSideGUID.codec ::
      uint16L
  ).xmap[squadPattern] (
    {
      case unk1 :: unk2 :: cguid :: x :: HNil =>
      SquadInfo(cguid) :: HNil
    },
    {
      case SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil =>
        1 :: true :: cguid.get :: 0 :: HNil
    }
  )
  //  type remove = Int :: Int :: Int :: Boolean :: Int :: Int :: HNil
  //  type updateLeader = Int :: Boolean :: Int :: Boolean :: String :: Int :: HNil
  //  type updateTask = Int :: Boolean :: Int :: Boolean :: String ::  Int :: HNil
  //  type updateLeaderSize = Int :: Boolean :: Int :: Boolean :: String :: Boolean :: Int :: Int :: Int :: HNil
  //  type updateTaskContinent = Int :: Boolean :: Int :: Boolean :: String :: Int :: Boolean :: PlanetSideGUID :: Int :: HNil

  implicit val codec : Codec[SquadHeader] = (
    ("action" | uint8L) >>:~ { action =>
      ("unk" | bool) >>:~ { unk =>
        initCodec.exmap[squadPattern](
          {
            case info :: HNil =>
              Attempt.Successful(info :: HNil)
          }, {
            case info :: HNil =>
              Attempt.Successful(info :: HNil)
          }
        )
    }}
    ).as[SquadHeader]

  implicit val alt_codec : Codec[SquadHeader] = (
    ("action" | uint8L) >>:~ { action =>
      ("unk" | bool) >>:~ { unk =>
        alt_initCodec.exmap[squadPattern](
          {
            case info :: HNil =>
              Attempt.Successful(info :: HNil)
          }, {
            case info :: HNil =>
              Attempt.Successful(info :: HNil)
          }
        )
    }}
    ).as[SquadHeader]
}

object SquadListing extends Marshallable[SquadListing] {
  implicit val codec : Codec[SquadListing] = (
    ("index" | uint8L) >>:~ { index =>
      conditional(index < 255,
        newcodecs.binary_choice(index == 0,
          "listing" | SquadHeader.codec,
          "listing" | SquadHeader.alt_codec)
      ) >>:~ { listing =>
        conditional(listing.isEmpty, bits).hlist //consume n < 8 bits padding the tail when decoding
      }
    }).as[SquadListing]
}

object ReplicationStreamMessage extends Marshallable[ReplicationStreamMessage] {
  implicit val codec : Codec[ReplicationStreamMessage] = (
    (("behavior" | uintL(3)) >>:~ { action =>
      conditional(action == 5, "init" | codec) :: //note: uses self
        conditional(action != 5, "unk" | bool)
    }) :+
      ("entries" | vector(SquadListing.codec))
    ).as[ReplicationStreamMessage]
}
