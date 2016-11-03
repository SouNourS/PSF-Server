// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._
import shapeless._

//this packet is limited mainly by byte-size

//continents are stored in this packet as 32-bit numbers instead of 16-bit; after the normal 16 bits, two bytes can be ignored

final case class SquadInfoOpt(leader : Option[String],
                           task : Option[String],
                           continent_guid : Option[PlanetSideGUID],
                           size : Option[Int],
                           capacity : Option[Int])

final case class SquadInfo(leader : String,
                           task : String,
                           continent_guid : PlanetSideGUID,
                           size : Int,
                           capacity : Int)

final case class SquadHeader(unk1 : Int,
                             unk2 : Boolean,
                             squad_guid : PlanetSideGUID,
                             info : SquadInfo)

final case class SquadListing(index : Int = 255,
                              listing : Option[SquadHeader] = None,
                              na : Option[Unit] = None)

final case class ReplicationStreamMessage(action : Int,
                                          init : Option[ReplicationStreamMessage],
                                          unk : Option[Boolean] = None,
                                          entries : Option[Vector[SquadListing]] = None)
  extends PlanetSideGamePacket {
  type Packet = ReplicationStreamMessage
  def opcode = GamePacketOpcode.ReplicationStreamMessage
  def encode = ReplicationStreamMessage.encode(this)
}

object SquadInfoOpt {
  //use: SquadInfoOpt(leader, task, continent_guid, size, capacity)
  def apply(leader : String, task : String, continent_guid : PlanetSideGUID, size : Int, capacity : Int) : SquadInfoOpt = {
    SquadInfoOpt(Some(leader), Some(task), Some(continent_guid), Some(size), Some(capacity))
  }

  //use: SquadInfoOpt(leader, None)
  def apply(leader : Option[String], task : String) : SquadInfoOpt = {
    SquadInfoOpt(leader, Some(task), None, None, None)
  }

  //use: SquadInfoOpt(None, task)
  def apply(leader : String, task : Option[String]) : SquadInfoOpt = {
    SquadInfoOpt(Some(leader), task, None, None, None)
  }

  def apply(continent_guid : PlanetSideGUID) : SquadInfoOpt = {
    SquadInfoOpt(None, None, Some(continent_guid), None, None)
  }

  //use: SquadInfoOpt(size, None)
  //we currently do not know the action codes that adjust squad capacity
  def apply(size : Int, capacity : Option[Int]) : SquadInfoOpt = {
    SquadInfoOpt(None, None, None, Some(size), None)
  }

  //use: SquadInfoOpt(None, capacity)
  //we currently do not know the action codes that adjust squad capacity
  def apply(size : Option[Int], capacity : Int) : SquadInfoOpt = {
    SquadInfoOpt(None, None, None, size, Some(capacity))
  }
}

object SquadInfo extends Marshallable[SquadInfo] {
  implicit val codec : Codec[SquadInfo] = (
    ("leader" | PacketHelpers.encodedWideString) ::
      ("task" | PacketHelpers.encodedWideString) ::
      ("continent_guid" | PlanetSideGUID.codec) ::
      ignore(16) ::
      ("size" | uint4L) ::
      ("capacity" | uint4L)
    ).as[SquadInfo]

  implicit val alt_codec : Codec[SquadInfo] = (
    ("leader" | PacketHelpers.encodedWideStringAligned(7)) ::
      ("task" | PacketHelpers.encodedWideString) ::
      ("continent_guid" | PlanetSideGUID.codec) ::
      ignore(16) ::
      ("size" | uint4L) ::
      ("capacity" | uint4L)
    ).as[SquadInfo]
}

object SquadHeader extends Marshallable[SquadHeader] {
  type initPattern = PlanetSideGUID :: SquadInfo :: HNil

  val init : Codec[initPattern] = (
      ("squad_guid" | PlanetSideGUID.codec) ::
        ("leader" | PacketHelpers.encodedWideString) ::
        ("task" | PacketHelpers.encodedWideString) ::
        ("continent_guid" | PlanetSideGUID.codec) ::
        //ignore(16) ::
        ("size" | uint4L) ::
        ("capacity" | uint4L)
    ).xmap[initPattern](
    {
      case sguid :: lead :: tsk :: cguid :: sz :: cap :: HNil =>
        sguid :: SquadInfo(lead, tsk, cguid, sz, cap) :: HNil
    },
    {
      case sguid :: SquadInfo(lead, task, cguid, sz, cap) :: HNil =>
        sguid :: lead :: task :: cguid :: sz :: cap :: HNil
    }
  )
  val alt_init : Codec[initPattern] = (
    ("squad_guid" | PlanetSideGUID.codec) ::
      ("leader" | PacketHelpers.encodedWideStringAligned(7)) ::
      ("task" | PacketHelpers.encodedWideString) ::
      ("continent_guid" | PlanetSideGUID.codec) ::
      //ignore(16) ::
      ("size" | uint4L) ::
      ("capacity" | uint4L)
    ).xmap[initPattern](
    {
      case sguid :: lead :: tsk :: cguid :: sz :: cap :: HNil =>
        sguid :: SquadInfo(lead, tsk, cguid, sz, cap) :: HNil
    },
    {
      case sguid :: SquadInfo(lead, task, cguid, sz, cap) :: HNil =>
        sguid :: lead :: task :: cguid :: sz :: cap :: HNil
    }
  )
  //  type remove = Int :: Int :: Int :: Boolean :: Int :: Int :: HNil
  //  type updateLeader = Int :: Boolean :: Int :: Boolean :: String :: Int :: HNil
  //  type updateTask = Int :: Boolean :: Int :: Boolean :: String ::  Int :: HNil
  //  type updateContinent = Int :: Boolean :: Int :: Boolean :: PlanetSideGUID :: Int :: HNil
  //  type updateSize = Int ::Boolean :: Int :: Boolean :: Int :: Int :: HNil
  //  type updateLeaderSize = Int :: Boolean :: Int :: Boolean :: String :: Boolean :: Int :: Int :: Int :: HNil
  //  type updateTaskContinent = Int :: Boolean :: Int :: Boolean :: String :: Int :: Boolean :: PlanetSideGUID :: Int :: HNil

  implicit val codec : Codec[SquadHeader] = (
    ("unk1" | uint8L) ::
      ("unk2" | bool) ::
      ("squad_guid" | PlanetSideGUID.codec) ::
      ("info" | SquadInfo.codec)
    ).as[SquadHeader]

  implicit val alt_codec : Codec[SquadHeader] = (
    ("unk1" | uint8L) ::
      ("unk2" | bool) ::
      ("squad_guid" | PlanetSideGUID.codec) ::
      ("info" | SquadInfo.alt_codec)
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
        conditional(listing.isEmpty, choice(ignore(1), ignore(0))).hlist
      }
    }).as[SquadListing]
}

object ReplicationStreamMessage extends Marshallable[ReplicationStreamMessage] {
  implicit val codec : Codec[ReplicationStreamMessage] = (
    ("action" | uintL(3)) >>:~ { action =>
      conditional(action == 5, "init" | codec) :: //note: calls self
        conditional(action != 5, "unk" | bool) ::
        conditional(action != 5, "entries" | vector(SquadListing.codec))
    }
    ).as[ReplicationStreamMessage]
}
