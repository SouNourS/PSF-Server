// Copyright (c) 2016 PSForever.net to present
import ChatService.{LeaveAll, NewMessage}
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.packet.game.ChatMsg
import net.psforever.types.ChatMessageType

object ChatService {
  case class Join(channel : String)
  case class LeaveAll()
  case class NewMessage(from : String, msg : ChatMsg)
}

/*
   /chat/
     - /zone/id
     - /platoon/id
     - /squad/id
     - /player/id
     - /broadcast/soi
     - /local/area
     -
 */

final case class ChatMessage(to : String, from : String, data : String)

class ChatEventBus extends ActorEventBus with SubchannelClassification {
  type Event = ChatMessage
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

/*object ChatPath {
  def apply(path : String) = new ChatPath(path)
}

class ChatPath(path: String) {
  def /(other : ChatPath) = this.path + "/" + other.path
}*/

class ChatService extends Actor {
  import ChatService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val chatEvents = new ChatEventBus

  /*val channelMap = Map(
    ChatMessageType.CMT_OPEN -> ChatPath("local")
  )*/

  def receive = {
    case Join(channel) =>
      val path = "/chat/" + channel
      val who = sender()

      log.info(s"${who} has joined ${path}")

      chatEvents.subscribe(who, path)
    case LeaveAll() =>
      chatEvents.unsubscribe(sender())
    case m @ NewMessage(from, msg) =>
//      log.info(s"NEW: ${m}")

      msg.messageType match {
        case ChatMessageType.CMT_OPEN =>
          chatEvents.publish(ChatMessage("/chat/local", from, msg.contents))
        case _ =>
      }
    case _ =>
  }
}
