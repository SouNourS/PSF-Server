// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.packet.game.{PlanetSideGUID, PlayerStateMessageUpstream}
import net.psforever.types.{ChatMessageType, Vector3}

object AvatarService {
  case class Join(channel : String)
  case class Leave()
  case class LeaveAll()
  case class PlayerStateMessage(msg : PlayerStateMessageUpstream)
}

/*
   /avatar/
     - /home2/id
     - /platoon/id
     - /squad/id
     - /player/id
     - /broadcast/soi
     - /local/area
     -
 */

//msg.avatar_guid, msg.pos, msg.vel, msg.unk1, msg.aim_pitch, msg.unk2, msg.is_crouching, msg.unk4, msg.is_cloaking
final case class AvatarMessage(to : String, avatar_guid : PlanetSideGUID, pos : Vector3, vel : Option[Vector3],
                               unk1 : Int, aim_pitch : Int, unk2 : Int,
                               is_crouching : Boolean, unk4 : Boolean, is_cloaking : Boolean)
//final case class AvatarMessage(avatar_guid : PlanetSideGUID, pos : Vector3, vel : Option[Vector3],
//                               unk1 : Int, aim_pitch : Int, unk2 : Int,
//                               is_crouching : Boolean, unk4 : Boolean, is_cloaking : Boolean)

class AvatarEventBus extends ActorEventBus with SubchannelClassification {
  type Event = AvatarMessage
  type Classifier = String

  protected def classify(event: Event): Classifier = event.to

  protected def subclassification = new Subclassification[Classifier] {
    def isEqual(x: Classifier, y: Classifier) = x == y
    def isSubclass(x: Classifier, y: Classifier) = x.startsWith(y)
  }

  protected def publish(event: Event, subscriber: Subscriber): Unit = {
    subscriber ! event
  }
}

/*object AvatarPath {
  def apply(path : String) = new AvatarPath(path)
}

class AvatarPath(path: String) {
  def /(other : AvatarPath) = this.path + "/" + other.path
}*/

class AvatarService extends Actor {
  import AvatarService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val AvatarEvents = new AvatarEventBus

  /*val channelMap = Map(
    AvatarMessageType.CMT_OPEN -> AvatarPath("local")
  )*/

  def receive = {
    case Join(channel) =>
      val path = "/Avatar/" + channel
      val who = sender()

      log.info(s"${who} has joined ${path}")

      AvatarEvents.subscribe(who, path)
    case Leave() =>
      AvatarEvents.unsubscribe(sender())
    case LeaveAll() =>
      AvatarEvents.unsubscribe(sender())
    case m @ PlayerStateMessage(msg) =>
//      log.info(s"NEW: ${m}")

      //PlayerStateMessageUpstream(avatar_guid, pos, vel, unk1, aim_pitch, unk2, seq_time, unk3, is_crouching, unk4, unk5, is_cloaking, unk6, unk7)
      //PlayerStateMessage(guid, pos, vel, facingYaw, facingPitch, facingYawUpper, unk1, is_crouching, is_jumping, unk2, is_cloaked)
//      msg.avatar_guid match {
//        case ChatMessageType.CMT_OPEN =>
          AvatarEvents.publish(AvatarMessage("/Avatar/home2", msg.avatar_guid, msg.pos, msg.vel, msg.unk1, msg.aim_pitch, msg.unk2, msg.is_crouching, msg.is_jumping, msg.is_cloaking))
//      AvatarEvents.publish(AvatarMessage(msg.avatar_guid, msg.pos, msg.vel, msg.unk1, msg.aim_pitch, msg.unk2, msg.is_crouching, msg.unk4, msg.is_cloaking))
//        case ChatMessageType.CMT_SQUAD =>
//          AvatarEvents.publish(ChatMessage("/Avatar/squad", from, msg.contents))
//        case _ =>
//      }
    case _ =>
  }
}
