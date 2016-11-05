// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.bits.BitVector
import scodec.{Codec, Err}
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
                             action2 : Option[Int],
                             info : SquadInfo)

final case class SquadListing(index : Int = 255,
                              listing : Option[SquadHeader] = None,
                              na : Option[BitVector] = None)

final case class ReplicationStreamMessage(behavior : Int,
                                          init : Option[ReplicationStreamMessage],
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

  //use: SquadInfo(leader, task, continent_guid, size, capacity, sguid)
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

  //use: SquadInfo(leader, size)
  def apply(leader : String, size : Int) : SquadInfo = {
    SquadInfo(Some(leader), None, None, Some(size), None)
  }

  //use: SquadInfo(task, continent_guid)
  def apply(task : String, continent_guid : PlanetSideGUID) : SquadInfo = {
    SquadInfo(None, Some(task), Some(continent_guid), None, None, None)
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
    ).xmap[squadPattern] (
    {
      case sguid :: lead :: tsk :: cguid :: x :: sz :: cap :: HNil =>
        SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil
    },
    {
      case SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil =>
        sguid.get :: lead.get :: tsk.get :: cguid.get :: 0 :: sz.get :: cap.get :: HNil
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
    ).xmap[squadPattern] (
    {
      case sguid :: lead :: tsk :: cguid :: x :: sz :: cap :: HNil =>
        SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil
    },
    {
      case SquadInfo(lead, tsk, cguid, sz, cap, sguid) :: HNil =>
        sguid.get :: lead.get :: tsk.get :: cguid.get :: 0 :: sz.get :: cap.get :: HNil
    }
  )

  val leaderCodec : Codec[squadPattern] = (
    bool ::
      ("leader" | PacketHelpers.encodedWideStringAligned(7))
    ).xmap[squadPattern] (
    {
      case x :: lead :: HNil =>
        SquadInfo(lead, None) :: HNil
    },
    {
      case SquadInfo(lead, None, None, None, None, None) :: HNil =>
        true :: lead.get :: HNil
    }
  )

  val taskOrContinentCodec : Codec[squadPattern] = (
    bool >>:~ { path =>
      conditional(path, "continent_guid" | PlanetSideGUID.codec) ::
        conditional(path, uint16L) ::
        conditional(!path, "task" | PacketHelpers.encodedWideStringAligned(7))
    }
    ).xmap[squadPattern] (
    {
      case true :: cguid :: Some(0) :: None :: HNil =>
        SquadInfo(cguid.get) :: HNil
      case false :: None :: None :: tsk :: HNil =>
        SquadInfo(None, tsk.get) :: HNil
    },
    {
      case SquadInfo(None, None, cguid, None, None, None) :: HNil =>
        true :: Some(cguid.get) :: Some(0) :: None :: HNil
      case SquadInfo(None, tsk, None, None, None, None) :: HNil =>
        false :: None :: None :: tsk :: HNil
    }
  )

  val sizeCodec : Codec[squadPattern] = (
    bool ::
      ("size" | uint4L)
    ).xmap[squadPattern] (
    {
      case false :: sz :: HNil =>
        SquadInfo(sz, None) :: HNil
    },
    {
      case SquadInfo(None, None, None, sz, None, None) :: HNil =>
        false :: sz.get :: HNil
    }
  )

  val leaderSizeCodec : Codec[squadPattern] = (
    bool ::
      ("leader" | PacketHelpers.encodedWideStringAligned(7)) ::
      uint4L ::
      ("size" | uint4L)
    ).xmap[squadPattern] (
    {
      case true :: lead :: 4 :: sz :: HNil =>
        SquadInfo(lead, sz) :: HNil
    },
    {
      case SquadInfo(lead, None, None, sz, None, None) ::HNil =>
        true :: lead.get :: 4 :: sz.get :: HNil
    }
  )

  val taskAndContinentCodec : Codec[squadPattern] = (
    bool ::
      ("task" | PacketHelpers.encodedWideStringAligned(7)) ::
      uintL(3) ::
      bool ::
      ("continent_guid" | PlanetSideGUID.codec) ::
      uint16L
    ).xmap[squadPattern] (
    {
      case false :: tsk :: 0 :: true :: cguid :: 0 :: HNil =>
        SquadInfo(tsk, cguid) :: HNil
    },
    {
      case SquadInfo(None, tsk, cguid, None, None, None) :: HNil =>
        false :: tsk.get :: 0 :: true :: cguid.get :: 0 :: HNil
    }
  )

  //TODO justify this mess
  val failureCodec : Codec[squadPattern] = peek(bool).xmap[squadPattern] (
    {
      case false => SquadInfo(None, None, None, None, None, None) :: HNil
      case _ => SquadInfo(None, None, None, None, None, None) :: HNil
    },
    {
      case SquadInfo(None, None, None, None, None, None) :: HNil => false
      case _ => false
    }
  )

  implicit val codec : Codec[SquadHeader] = (
    ("action" | uint8L) >>:~ { action =>
      ("unk" | bool) >>:~ { unk =>
        conditional(action != 131 && !unk, "action2" | uintL(3)) >>:~ { action2 =>
          selectCodec(action, unk, action2)
        }
      }
    }
    ).as[SquadHeader]

  implicit val alt_codec : Codec[SquadHeader] = (
    ("action" | uint8L) >>:~ { action =>
      ("unk" | bool) >>:~ { unk =>
        conditional(action != 131, "action2" | uintL(3)) >>:~ { action2 =>
          selectCodec(action, unk, action2, alt_initCodec)
        }
      }
    }
    ).as[SquadHeader]

  def selectCodec(action : Int, unk : Boolean, action2 : Option[Int], init : Codec[squadPattern] = initCodec) : Codec[squadPattern] = {
    if(action2.isDefined) {
      val action2Val = action2.get
      if(action == 128 && unk) {
        if(action2Val == 0)
          return leaderCodec
        else if(action2Val == 1)
          return taskOrContinentCodec
        else if(action2Val == 2)
          return sizeCodec
      }
      else if(action == 129 && !unk) {
        if(action2Val == 0)
          return leaderSizeCodec
        else if(action2Val == 1)
          return taskAndContinentCodec
      }
    }
    else {
      if(action == 131 && !unk)
        return init
    }

    //TODO better error handling here?
    Err("requesting an unknown codec")
    failureCodec
  }
}

object SquadListing extends Marshallable[SquadListing] {
  implicit val codec : Codec[SquadListing] = (
    ("index" | uint8L) >>:~ { index =>
      conditional(index < 255,
        newcodecs.binary_choice(index == 0,
          "listing" | SquadHeader.codec,
          "listing" | SquadHeader.alt_codec)
      ) ::
        conditional(index == 255, bits).hlist //consume n < 8 bits padding the tail entry, else vector will try to operate on invalid data
    }).as[SquadListing]
}

object ReplicationStreamMessage extends Marshallable[ReplicationStreamMessage] {
  implicit val codec : Codec[ReplicationStreamMessage] = (
    (("behavior" | uintL(3)) >>:~ { behavior =>
      conditional(behavior == 5, "init" | codec) :: //note: uses self
        conditional(behavior != 5 && behavior != 2, "unk" | bool)
    }) :+
      ("entries" | vector(SquadListing.codec))
    ).as[ReplicationStreamMessage]
}
