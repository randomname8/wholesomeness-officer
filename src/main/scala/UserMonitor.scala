import akka.actor.{Props, LoggingFSM, FSM}
import java.time.Instant
import scala.concurrent.duration._
import sx.blah.discord.handle.obj.{IChannel, IMessage, IUser}

import UserMonitor._

class UserMonitor(user: IUser, channel: IChannel, requiredReports: Int, actionHandler: ActionHandler, timeoutSequence: Seq[FiniteDuration]) extends LoggingFSM[State, Data] {
  startWith(Clean, Data())
  
  when(Clean) {
    case Event(r@Reported(msg, by), data) => 
      log.info(s"Got a report for ${user.getName} in channel ${channel.getName}, starting to monitor him.")
      actionHandler.warnUser(user, msg)
      goto(Monitoring) using data.copy(reports = Seq(r))
  }
  
  when(Monitoring, stateTimeout = 5.minutes) {
    case NewReportEvent(r: Reported, data @ Data(reports, pastTimeouts)) if reports.size == (requiredReports - 1) =>
      log.info(s"User ${user.getName} in channel ${channel.getName} got enough reports, timing him out.")
      val now = Instant.now()
      val _24HoursBefore = now.minusSeconds(3600 * 24)
      val consideredTimeouts = pastTimeouts.dropWhile(_.when isBefore _24HoursBefore) //only consider the past 24 hours
      
      val nextTimeout = TimedOutData(now, timeoutSequence(consideredTimeouts.length min (timeoutSequence.length - 1)))
      val nextData = Data(reports :+ r, consideredTimeouts :+ nextTimeout)
      
      actionHandler.muteUser(user, channel, nextTimeout.duration, nextData.reports)
      goto(TimedOut) forMax nextTimeout.duration using nextData
      
    case NewReportEvent(r: Reported, data) =>
      log.info(s"Received another report for ${user.getName} in channel ${channel.getName}, total: ${data.reports.length + 1}")
      stay using data.copy(data.reports :+ r)


    //if there's only one report left, go back to clean
    case Event(StateTimeout, Data(Seq(_), pastTimeouts)) => goto(Clean) using Data(pastTimeouts = pastTimeouts)
    //otherwise reduce the reports by one
    case Event(StateTimeout, data) => stay using data.copy(reports = data.reports.drop(1))
  }
  
  val unmuteHandler: StateFunction = {
    case Event(Unmute(msg), data) =>
      log.info(s"Unmuting ${user.getName} in channel ${channel.getName} under request of ${msg.getAuthor.getName}")
      actionHandler.unmuteUser(user, channel, Some(msg))
      goto(Clean) using Data(Seq.empty, data.pastTimeouts.init) //discard last timeout because it got appealed
      
    case Event(StateTimeout, data) =>
      log.info(s"User ${user.getName} in channel ${channel.getName} completed his time out. Unmuting him.")
      actionHandler.unmuteUser(user, channel, None)
      goto(Clean) using data.copy(reports = Seq.empty)
  }
  
  when(TimedOut)(unmuteHandler orElse {
    case Event(Appealed(msg), data) =>
      actionHandler.appealUser(user, channel, msg)
      //calculate remaning time in timeout to specify a timer to the Appealing state
      val TimedOutData(when, duration) = data.pastTimeouts.last
      val remainingTime = duration - (Instant.now.toEpochMilli - when.toEpochMilli).millis
      log.info(s"User ${user.getName} in channel ${channel.getName} requests an appeal. Remaining time $remainingTime")
      goto(Appealing) forMax remainingTime
  })
  
  when(Appealing) (unmuteHandler orElse {
    case Event(Appealed(msg), data) =>
      actionHandler.appealingProcessAlreadyStarted(user, msg)
      stay
  })
  
  whenUnhandled {
    case Event(cmd @ (_: Appealed | _: Unmute), _) =>
      actionHandler.notifyUserNotTimedOut(user, cmd.asInstanceOf[Command].message, channel)
      stay
      
    case Event(IsMuted, data) =>
      stateName match {
        case TimedOut | Appealing => sender ! data.pastTimeouts.last
        case _ => sender ! ()
      }
      stay
  }
}
object UserMonitor {
  def props(user: IUser, channel: IChannel, requiredReports: Int, actionHandler: ActionHandler, timeOutSequence: Seq[FiniteDuration]) = 
    Props(new UserMonitor(user, channel, requiredReports, actionHandler, timeOutSequence))
  
  object NewReportEvent {
    def unapply(evt: FSM.Event[Data]): Option[(Reported, Data)] = evt match {
      case FSM.Event(r @ Reported(msg, by), data @ Data(reports, _)) /* if !reports.exists(_.getAuthor.getLongID == msg.getAuthor.getLongID) */ =>
        Some(r -> data)
      case _ => None
    }
  }
  
  sealed trait State
  case object Clean extends State
  case object Monitoring extends State
  case object TimedOut extends State
  case object Appealing extends State
  
  case class Data(reports: Seq[Reported] = Seq.empty, pastTimeouts: Seq[TimedOutData] = Seq.empty)
  case class TimedOutData(when: Instant, duration: FiniteDuration)
  
  sealed trait Command { def message: IMessage }
  case class Reported(message: IMessage, by: IUser) extends Command
  case class Appealed(message: IMessage) extends Command
  case class Unmute(message: IMessage) extends Command
  
  object IsMuted

  trait ActionHandler {
    def muteUser(user: IUser, channel: IChannel, duration: FiniteDuration, reports: Seq[Reported]): Unit
    def unmuteUser(user: IUser, channel: IChannel, message: Option[IMessage]): Unit
    def warnUser(user: IUser, message: IMessage): Unit
    def appealUser(user: IUser, channel: IChannel, message: IMessage): Unit
    def appealingProcessAlreadyStarted(user: IUser, message: IMessage): Unit
    def notifyUserNotTimedOut(user: IUser, message: IMessage, channel: IChannel): Unit
  }
}