package chousen.db

import org.scalatest.{Matchers, WordSpec}


class SimpleDBCheck extends WordSpec with Matchers {

  "SimpleDB Check" should {

    "Ensure save and load works" ignore {
//      lazy val mongo = new MongoDatastore(
//        "mongodb://chousen:chousen@ds123080.mlab.com:23080/?authSource=heroku_rm14s281&authMechanism=SCRAM-SHA-1",
//        "heroku_rm14s281",
//        "chousen")
//      val access = new MongoGameAccess(mongo)
//
//      val s = access.storeGame(GameStateGenerator.staticGameState.copy(uuid = UUID.randomUUID())).unsafeRun()
//
//      print(s.uuid)
//
//      access.withGame(s.uuid){ x =>
//        x.uuid shouldBe(s.uuid)
//        x.player.name shouldBe s.player.name
//
//        Created("")
//      }.unsafeRun()
    }


  }
}
