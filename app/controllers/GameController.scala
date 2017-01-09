package controllers

import javax.inject.Inject

import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, Controller}


class GameController @Inject()(val messagesApi: MessagesApi) extends Controller with I18nSupport {

  def initGame = Action { implicit request =>
    Ok(views.html.base(views.html.makeChar(userForm)))
  }

  val userForm = Form(
    mapping(
      "name" -> text
    )(UserData.apply)(UserData.unapply)
  )

  def createGame = Action { implicit request =>
    userForm.bindFromRequest().fold(
      hasErrors => BadRequest(views.html.makeChar(hasErrors)),
      user => Ok(s"User ${user.name} created successfully"))
  }
}

case class UserData(name: String)