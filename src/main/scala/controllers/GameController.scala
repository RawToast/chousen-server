package controllers

import java.util.UUID

//import play.api.mvc.{Action, Controller}
//import io.circe.generic.auto._
//import io.circe.syntax._
//import play.api.libs.circe.Circe
//
//class GameController extends Controller with Circe {
//
//
//  def initGame() = Action(circe.json[CreateRequest]) { implicit request =>
//    val body: CreateRequest = request.body
//
//    ???
//  }
//
//  def loadGame = Action { implicit request =>
//    ???
//  }
//}

case class CreateRequest(name:String)

case class CreateResponse(id: UUID)

case class LoadRequest(id: UUID)
