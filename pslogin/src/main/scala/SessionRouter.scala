// Copyright (c) 2016 PSForever.net to present
import java.io.{FileWriter}
import java.net.InetSocketAddress

import akka.actor._
import org.log4s.MDC
import scodec.bits._

import scala.collection.mutable
//import MDCContextAware.Implicits._
//import akka.actor.MDCContextAware.MdcMsg
import akka.actor.SupervisorStrategy.Stop
import net.psforever.packet.PacketCoding
import net.psforever.packet.control.ConnectionClose

import scala.concurrent.duration._

sealed trait SessionRouterAPI
final case class RawPacket(data : ByteVector) extends SessionRouterAPI
final case class ResponsePacket(data : ByteVector) extends SessionRouterAPI
final case class DropSession(id : Long, reason : String) extends SessionRouterAPI
final case class SessionReaper() extends SessionRouterAPI

case class SessionPipeline(nameTemplate : String, props : Props)

/**
  * Login sessions are divided between two actors. The crypto session actor transparently handles all of the cryptographic
  * setup of the connection. Once a correct crypto session has been established, all packets, after being decrypted
  * will be passed on to the login session actor. This actor has important state that is used to maintain the login
  * session.
  *
  *                      > PlanetSide Session Pipeline <
  *
  *            read()                  route                decrypt
  * UDP Socket -----> [Session Router] -----> [Crypto Actor] -----> [Session Actor]
  *      ^              |          ^           |        ^                 |
  *      |     write()  |          |  encrypt  |        |   response      |
  *      +--------------+          +-----------+        +-----------------+
  **/
class SessionRouter(role : String, pipeline : List[SessionPipeline]) extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger(self.path.name)

  import scala.concurrent.ExecutionContext.Implicits.global
  val sessionReaper = context.system.scheduler.schedule(10 seconds, 5 seconds, self, SessionReaper())

  var xActiveSession = 0
 // val FileToWrite = "xActiveSession"
  val FileToWrite = "/www/status.htm"

  val idBySocket = mutable.Map[InetSocketAddress, Long]()
  val sessionById = mutable.Map[Long, Session]()
  val sessionByActor = mutable.Map[ActorRef, Session]()
  val closePacket = PacketCoding.EncodePacket(ConnectionClose()).require.bytes

  var sessionId = 0L // this is a connection session, not an actual logged in session ID
  var inputRef : ActorRef = ActorRef.noSender

  override def supervisorStrategy = OneForOneStrategy() { case _ => Stop }

  override def preStart = {
    log.info(s"SessionRouter started...ready for ${role} sessions")
  }

  def receive = initializing

  def initializing : Receive = {
    case Hello() =>
      inputRef = sender()
      context.become(started)
    case default =>
      log.error(s"Unknown message $default. Stopping...")
      context.stop(self)
  }

  override def postStop() = {
    sessionReaper.cancel()
  }

  def started : Receive = {
    case recv @ ReceivedPacket(msg, from) =>
      var session : Session = null

      if(!idBySocket.contains(from)) {
        session = createNewSession(from)
      }
      else {
        val id = idBySocket{from}
        session = sessionById{id}
      }

      if(session.state != Closed()) {
        MDC("sessionId") = session.sessionId.toString
          log.trace(s"RECV: ${msg} -> ${session.getPipeline.head.path.name}")
          session.receive(RawPacket(msg))
        MDC.clear()
      }
    case ResponsePacket(msg) =>
      val session = sessionByActor.get(sender())

      if(session.isDefined) {
        if(session.get.state != Closed()) {
          MDC("sessionId") = session.get.sessionId.toString
            log.trace(s"SEND: ${msg} -> ${inputRef.path.name}")
            session.get.send(msg)
          MDC.clear()
        }
      } else {
        log.error("Dropped old response packet from actor " + sender().path.name)
      }
    case DropSession(id, reason) =>
      val session = sessionById.get(id)

      if(session.isDefined) {
        removeSessionById(id, reason, graceful = true)
      } else {
        log.error(s"Requested to drop non-existent session ID=$id from ${sender()}")
      }
    case SessionReaper() =>
      sessionById.foreach { case (id, session) =>
        log.debug(session.toString)
        if(session.getState == Closed()) {
          // clear mappings
          session.getPipeline.foreach(sessionByActor remove)
          sessionById.remove(id)
          idBySocket.remove(session.socketAddress)
          log.debug(s"Reaped session ID=$id")
        } else if(session.timeSinceLastInboundEvent > 10000) {
          removeSessionById(id, "session timed out (inbound)", graceful = false)
        } else if(session.timeSinceLastOutboundEvent > 4000) {
          removeSessionById(id, "session timed out (outbound)", graceful = true) // tell client to STFU
        }
      }
    case Terminated(actor) =>
      val terminatedSession = sessionByActor.get(actor)

      if(terminatedSession.isDefined) {
        removeSessionById(terminatedSession.get.sessionId, s"${actor.path.name} died",
          graceful = true)
      } else {
        log.error("Received an invalid actor Termination from " + actor.path.name)
      }
    case default =>
      log.error(s"Unknown message $default from " + sender().path)
  }

  def createNewSession(address : InetSocketAddress) = {
    val id = newSessionId
    val session = new Session(id, address, inputRef, pipeline)

    // establish mappings for easy lookup
    idBySocket{address} = id
    sessionById{id} = session

    session.getPipeline.foreach { actor =>
      sessionByActor{actor} = session
    }

    log.info(s"New session ID=${id} from " + address.toString)

    if(role == "World") {
      xActiveSession += 1
//      log.info("SessionRouter _ xActiveSession : " + xActiveSession)
      val fw = new FileWriter(FileToWrite, false)
      fw.write("<html><head></head><body><div class=\"servername\">PSForever Test Server</div><div class=\"location\">EU-France</div>" +
        "<div class=\"playercount\">" + xActiveSession.toString + "</div><div class=\"status\">ON</div></body></html>")
      fw.close()
    }
    session
  }

  def removeSessionById(id : Long, reason : String, graceful : Boolean) : Unit = {
    val sessionOption = sessionById.get(id)

    if(sessionOption.isEmpty)
      return

    val session : Session = sessionOption.get

    if(graceful) {
      for(i <- 0 to 5) {
        session.send(closePacket)
      }
    }

    // kill all session specific actors
    session.dropSession(graceful)
    log.info(s"Dropping session ID=${id} (reason: $reason)")

    if(role == "World") {
      xActiveSession -= 1
//      log.info("SessionRouter _ xActiveSession : " + xActiveSession)
      val fw = new FileWriter(FileToWrite, false)
      fw.write("<html><head></head><body><div class=\"servername\">PSForever Test Server</div><div class=\"location\">EU-France</div>" +
        "<div class=\"playercount\">" + xActiveSession.toString + "</div><div class=\"status\">ON</div></body></html>")
      fw.close()
    }
  }

  def newSessionId = {
    val oldId = sessionId
    sessionId += 1
    oldId
  }
}
