import sx.blah.discord.handle.obj.{IRole, IUser}

object D4jExtensions {

  implicit class UserExt(val user: IUser) extends AnyVal {
    def hasRole(role: IRole) = user.getRolesForGuild(role.getGuild).contains(role)
  }
}
