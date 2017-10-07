import akka.actor.{Props, FSM}
import java.time.Instant
import scala.concurrent.duration._
import sx.blah.discord.handle.obj.{IChannel, IMessage, IUser}

import UserMonitor._

class UserMonitor(user: IUser, channel: IChannel, requiredReports: Int, actionHandler: ActionHandler) extends FSM[State, Data] {
  startWith(Clean, Data())
  
  when(Clean) {
    case Event(Reported(msg), data) => goto(Monitoring) using data.copy(reports = Seq(msg))
  }
  
  when(Monitoring, stateTimeout = 5.minutes) {
    case NewReportEvent(Reported(msg), data @ Data(reports, pastTimeouts)) if reports.size == (requiredReports - 1) =>
      val now = Instant.now()
      val _24HoursBefore = now.minusSeconds(3600 * 24)
      val consideredTimeouts = pastTimeouts.dropWhile(_.when isBefore _24HoursBefore) //only consider the past 24 hours
      
      val nextTimeout = TimedOutData(now, TimeOutSequence(consideredTimeouts.length max TimeOutSequence.length))
      val nextData = Data(reports :+ msg, consideredTimeouts :+ nextTimeout)
      
      actionHandler.muteUser(user, channel, nextTimeout.duration, nextData.reports)
      goto(TimedOut) forMax nextTimeout.duration using nextData
      
    case NewReportEvent(Reported(msg), data) =>
      if (data.reports.length == 1) actionHandler.warnUser(user, msg)
      stay using data.copy(data.reports :+ msg)


    //if there's only one report left, go back to clean
    case Event(StateTimeout, Data(Seq(_), pastTimeouts)) => goto(Clean) using Data(pastTimeouts = pastTimeouts)
    //otherwise reduce the reports by one
    case Event(StateTimeout, data) => stay using data.copy(reports = data.reports.drop(1))
  }
  
  val unmuteHandler: StateFunction = {
    case Event(Unmute(msg), data) =>
      actionHandler.unmuteUser(user, channel, msg)
      goto(Clean) using Data(Seq.empty, data.pastTimeouts.init) //discard last timeout because it got appealed
      
    case Event(StateTimeout, data) => goto(Clean) using data.copy(reports = Seq.empty)
  }
  
  when(TimedOut)(unmuteHandler orElse {
    case Event(Appealed(msg), data) =>
      actionHandler.appealUser(user, channel, msg)
      //calculate remaning time in timeout to specify a timer to the Appealing state
      val TimedOutData(when, duration) = data.pastTimeouts.last
      val remainingTime = duration - (Instant.now.toEpochMilli - when.toEpochMilli).millis
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
  def props(user: IUser, channel: IChannel, requiredReports: Int, actionHandler: ActionHandler) = Props(new UserMonitor(user, channel, requiredReports, actionHandler))
  
  object NewReportEvent {
    def unapply(evt: FSM.Event[Data]): Option[(Reported, Data)] = evt match {
      case FSM.Event(r @ Reported(msg), data @ Data(reports, _)) if !reports.exists(_.getAuthor.getLongID == msg.getAuthor.getLongID) =>
        Some(r -> data)
      case _ => None
    }
  }
  
  sealed trait State
  case object Clean extends State
  case object Monitoring extends State
  case object TimedOut extends State
  case object Appealing extends State
  
  case class Data(reports: Seq[IMessage] = Seq.empty, pastTimeouts: Seq[TimedOutData] = Seq.empty)
  case class TimedOutData(when: Instant, duration: FiniteDuration)
  
  sealed trait Command { def message: IMessage }
  case class Reported(message: IMessage) extends Command
  case class Appealed(message: IMessage) extends Command
  case class Unmute(message: IMessage) extends Command
  
  object IsMuted

  val TimeOutSequence = Array(5.minutes, 15.minutes, 1.hour, 3.hours, 24.hours)
  
  trait ActionHandler {
    def muteUser(user: IUser, channel: IChannel, duration: FiniteDuration, reports: Seq[IMessage]): Unit
    def unmuteUser(user: IUser, channel: IChannel, message: IMessage): Unit
    def warnUser(user: IUser, message: IMessage): Unit
    def appealUser(user: IUser, channel: IChannel, message: IMessage): Unit
    def appealingProcessAlreadyStarted(user: IUser, message: IMessage): Unit
    def notifyUserNotTimedOut(user: IUser, message: IMessage, channel: IChannel): Unit
  }
}