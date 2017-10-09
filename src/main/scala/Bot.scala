import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import java.util.EnumSet
import regex._
import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Success, Failure}
import sx.blah.discord.Discord4J
import sx.blah.discord.api.ClientBuilder
import sx.blah.discord.api.events.{Event, IListener}
import sx.blah.discord.handle.impl.events.ReadyEvent
import sx.blah.discord.handle.impl.events.guild.channel.ChannelCreateEvent
import sx.blah.discord.handle.impl.events.guild.channel.ChannelDeleteEvent
import sx.blah.discord.handle.impl.events.guild.channel.message.MessageReceivedEvent
import sx.blah.discord.handle.impl.events.guild.role.RoleDeleteEvent
import sx.blah.discord.handle.impl.obj.ReactionEmoji
import sx.blah.discord.handle.obj.{IMessage, IUser, IChannel, IRole, Permissions}
import sx.blah.discord.util.RequestBuffer

import D4jExtensions._

object Bot extends App with UserMonitor.ActionHandler {
  Discord4J.disableAudio()

  val botConfig = ConfigFactory.parseFile(new java.io.File("conf/bot.config"))
  lazy val theGuild = client.getGuilds.get(0)
  val muteRolPerChannel = TrieMap[IChannel, IRole]()
  val userMonitors = TrieMap[IUser, TrieMap[IChannel, ActorRef]]()
  lazy val auditChannel = theGuild.getChannelByID(botConfig.getLong("bot.auditChannel"))
  lazy val moderatorRole = theGuild.getRolesByName(botConfig.getString("bot.moderatorRol")).get(0)
  val requiredReports = botConfig.getInt("bot.requiredReports")
  val timeoutsSequence = botConfig.getStringList("bot.timeoutsSequence").asScala.map(Duration.apply).collect { case d: FiniteDuration => d }.toSeq
  val reportExpiration = Duration(botConfig.getString("bot.reportExpiration")).asInstanceOf[FiniteDuration]
  
  val actorSystem = ActorSystem()
  
  val client = new ClientBuilder().withToken(botConfig.getString("bot.token")).
    setMaxMessageCacheCount(100).
    withMinimumDispatchThreads(1).
    withMaximumDispatchThreads(5).
    withIdleDispatchThreadTimeout(1, MINUTES).
    registerListener(DiscordListener).
    login()

  implicit class IteratorExt[T](val i: Iterator[T]) extends AnyVal {
    def nextOpt(): Option[T] = if (i.hasNext) Some(i.next) else None
  }
  
  
  object DiscordListener extends IListener[Event] {
    
    def handle(event: Event) = event match {
      case evt: ReadyEvent =>
        if (client.getGuilds.size != 1) {
          println("This bot can only be registered to one guild at a time")
          sys.exit(1)
        }
        if (auditChannel == null) {
          println(s"Configured audit channel ${botConfig.getLong("bot.auditChannel")} not found")
          sys.exit(1)
        }
        if (moderatorRole == null) {
          println(s"Configured moderator rol ${botConfig.getLong("bot.moderatorRol")} not found")
          sys.exit(1)
        }
        theGuild.getChannels.asScala foreach { channel =>
          theGuild.getRolesByName(s"${channel.getStringID}-muted").asScala.headOption match {
            case Some(role) => muteRolPerChannel(channel) = role
            case _ => muteRolPerChannel(channel) = setupMuteRolForChannel(channel)
          }
        }
        println(Console.GREEN + s"""Initialized.
  Audit channel: ${auditChannel.getName}
  Moderator role: ${moderatorRole.getName}
  Timeout sequence: ${timeoutsSequence mkString ", "}
  Required reports for timeout: $requiredReports
  Report expiration: $reportExpiration""" + Console.RESET)
        
      case evt: ChannelCreateEvent => muteRolPerChannel(evt.getChannel) = setupMuteRolForChannel(evt.getChannel)
      case evt: ChannelDeleteEvent => muteRolPerChannel -= evt.getChannel
        
      case evt: RoleDeleteEvent if muteRolPerChannel.values.exists(_ == evt.getRole) =>
        try {
          val channel = theGuild.getChannelsByName(evt.getRole.getName).get(0)
          auditChannel.sendMessage("Please don't delete the roles I created! I need those for proper functioning!\nI'll recreate it now.")
          setupMuteRolForChannel(channel)
        } catch {
          case NonFatal(e) => 
            println(s"Could not find the channel for the rol ${evt.getRole.getName}?")
            e.printStackTrace()
        }
        
      case evt: MessageReceivedEvent =>
        val content = evt.getMessage.getContent.replaceFirst(raw"""^\Q${client.getOurUser.mention(false)}\E\s+""", "")
        val mentionsMe = content.length != evt.getMessage.getContent.length
        
        if (mentionsMe || evt.getChannel.isPrivate) {
          commands.find(_.action(evt.getMessage).isDefinedAt(content)) match {
            case Some(command) => command.action(evt.getMessage)(content)
            case _ => evt.getMessage.reply(s"Sorry, I don't know the command: ${evt.getMessage.getContent}")
          }
        }
        
      case _ =>
    }
  }
  
  val commands = collection.mutable.ListBuffer[Command]()
  case class Command(name: String, description: String, requiresModerator: Boolean = false)(val action: IMessage => PartialFunction[String, Any]) { commands += this }
  
  Command("report <messageId>", "Reports a message by a user. USE RESPONSIBLY.")(msg => {
      case gr"""report $idStr(\d+)""" if msg.getChannel.isPrivate =>
        val msgId = idStr.toLong

        Option(theGuild.getMessageByID(msgId)) orElse 
        theGuild.getChannels.asScala.iterator.flatMap(c => Option(c.getMessageByID(msgId))).nextOpt() match {
          case None => msg.reply("Message not found.")
          case Some(reportedMsg) if reportedMsg.getAuthor.getLongID == msg.getAuthor.getLongID => msg.reply("You are trying to report yourself.")
          case Some(reportedMsg) =>
            val reportedUser = reportedMsg.getAuthor
            val monitor = userMonitors.getOrElseUpdate(reportedUser, TrieMap()).getOrElseUpdate(
              reportedMsg.getChannel, actorSystem.actorOf(UserMonitor.props(reportedUser, reportedMsg.getChannel, requiredReports, Bot.this, timeoutsSequence, reportExpiration)))
            monitor ! UserMonitor.Reported(reportedMsg, msg.getAuthor)
            msg.reply(s"User ${reportedUser.getName} reported")
        }
    })
  
  Command("appeal[ channelId]", "If you believe you have been wrongly timed out.")(msg => {
      case gr"""appeal(?: $idStr(\d+))?""" if msg.getChannel.isPrivate =>
        val chosenChannel = idStr.map {
          case str @ gr"\\d+" => Option(theGuild.getChannelByID(str.toLong))
          case other => theGuild.getChannelsByName(other).asScala.headOption
        }

        chosenChannel match {
          case Some(Some(chosen)) => 
            userMonitors.get(msg.getAuthor).flatMap(_.get(chosen)).fold[Unit](
              msg.reply(s"You are not timed out in channel ${chosen.getName}"))(_ ! UserMonitor.Appealed(msg))
          case Some(None) => msg.reply("Channel not found")
          case _ =>
            userMonitors.get(msg.getAuthor).fold[Unit](msg.reply("You are not timed out")) { timeouts =>
              timeouts.size match {
                case 1 => timeouts.head._2 ! UserMonitor.Appealed(msg)
                case 0 => msg.reply("You are not timed out")
                case other => msg.reply("You're timed out in multiple channels, you need to tell me for which channel you wish to appeal by doing `appeal channelId` where `channelId` can be either the snowflake id of the channel, or the channel name (assuming it is unique)")
              }
            }
        }
        
    })
  
  Command("unmute <userId> <channelId>", "Unmutes a timed out user", requiresModerator = true)(msg => {
      case gr"""unmute $userStrId(\d+) $channelStrId(\d+)""" if msg.getAuthor.hasRole(moderatorRole) && msg.getChannel == auditChannel =>
        val validation = 
          for {
            user <- Option(theGuild.getUserByID(userStrId.toLong)) toRight "User not found"
            channel <- Option(theGuild.getChannelByID(channelStrId.toLong)) toRight "Channel not found"
          } yield {
            userMonitors.get(user) flatMap (_.get(channel)) match {
              case Some(monitor) => monitor ! UserMonitor.Unmute(msg)
              case _ => notifyUserNotTimedOut(user, msg, channel)
            }
          }
        
        validation.left.foreach(res => msg.reply(res))
        
    })
  
  Command("list muted", "Shows all the people that are muted per channel", requiresModerator = true)(msg => {
      case "list muted" if msg.getAuthor.hasRole(moderatorRole) && msg.getChannel == auditChannel =>
        implicit val requestTimeout = Timeout(1.second)
        
        val flattenedMonitors = for {
          (user, monitors) <- userMonitors
          (channel, monitor) <- monitors
        } yield (monitor ? UserMonitor.IsMuted) map ((user, channel, _))
        
        Future.sequence(flattenedMonitors).onComplete {
          case Success(flattenedMonitors) =>
            val timedOutUsers = flattenedMonitors.collect { case (user, channel, data: UserMonitor.TimedOutData) => (user, channel, data) }.toVector
            val totalTimedOuts = timedOutUsers.size
            
            val report = new collection.mutable.ArrayBuffer[String](10)
            report += s"Total timed out users **$totalTimedOuts**."
            
            for {
              (channel, users) <- timedOutUsers.groupBy(_._2.getName)
              _ = report += s"Channel **$channel**:"
              (user, _, data) <- users
            } report += s"  ${user.getName} (${user.getNicknameForGuild(theGuild)}) until ${data.when.plusMillis(data.duration.toMillis)}"
            
            val (messages, remaining) = report.foldLeft(new collection.mutable.ArrayBuffer[String](report.length) -> new StringBuilder) {
              case ((messages, currentMessage), elem) => 
                if (currentMessage.size + (elem.length + 1) < 2000) {
                  currentMessage append elem append "\n"
                } else {
                  messages += currentMessage.result()
                  currentMessage.clear()
                }
                messages -> currentMessage
            }
            if (remaining.nonEmpty) messages += remaining.result()
            messages foreach (part => RequestBuffer.request(() => msg.reply(part)))
            
          case Failure(ex) => msg.reply("Something went wrong:\n```" + ex.getStackTrace.mkString("\n") + "```")
        }
    })
  
  Command("help", "Prints this help message")(msg => {
      case "help" =>
        val toShow = if (msg.getAuthor.hasRole(moderatorRole)) commands else commands.filterNot(_.requiresModerator)
        
        val maxCmdWidth = toShow.map(_.name.length).max
        val helpString = new StringBuilder
        toShow foreach (c => helpString.append(c.name.padTo(maxCmdWidth, ' ')).append(" - ").append(c.description).append("\n"))
        msg.reply("```\n" + helpString.toString + "```")
    })
  
  def setupMuteRolForChannel(channel: IChannel): IRole = {
    val role = theGuild.createRole()
    role.edit(java.awt.Color.red, false, s"${channel.getStringID}-muted", EnumSet.noneOf(classOf[Permissions]), true)
    channel.overrideRolePermissions(role, EnumSet.noneOf(classOf[Permissions]), EnumSet.of(Permissions.SEND_MESSAGES, Permissions.SEND_TTS_MESSAGES))
    role
  }
  override def muteUser(user: IUser, channel: IChannel, duration: FiniteDuration, reports: Seq[UserMonitor.Reported]) = {
    user.addRole(muteRolPerChannel(channel))
    auditChannel.sendMessage(s"User ${user.getName} muted in channel ${channel.mention} for $duration. Reported by\n" + reports.map(_.by.getName).mkString("\n"))
    if (!user.isBot) {
      user.getOrCreatePMChannel().sendMessage(s"You have been muted in ${channel.mention} for $duration after repeated reports.\n" +
                                              "If you consider this to be wrong, you can ask for an appealing processing by sending to me" +
                                              s"```appeal ${channel.getStringID}```")
    }
  }
  override def unmuteUser(user: IUser, channel: IChannel, message: Option[IMessage]) = {
    user.removeRole(muteRolPerChannel(channel))
    val descr = message.fold("on timeout expiration.")(m => s" on request of ${m.getAuthor.mention}")
    auditChannel.sendMessage(s"User ${user.getName} unmuted $descr.")
    
  }
  override def warnUser(user: IUser, message: IMessage) = {
    message.addReaction(ReactionEmoji of "⚠")
  }
  def appealUser(user: IUser, channel: IChannel, message: IMessage): Unit = {
    message.reply("Appeal process initiated. You'll have to wait for the team of moderators to review your case.")
    auditChannel.sendMessage(s"${moderatorRole.mention}, user ${user.getName} requested an appeal to the timeout he received in channel ${channel.mention}.")
  }
  override def notifyUserNotTimedOut(user: IUser, message: IMessage, channel: IChannel) = {
      message.reply(s"User ${user.getName} is not timed out in channel ${channel.getName}")
  }
  override def appealingProcessAlreadyStarted(user: IUser, message: IMessage): Unit = {
    message.reply("The appealing process has already been started.")
  }
}
