package chousen.api.data

import java.util.UUID

import io.circe.generic.extras.semiauto._
import io.circe.generic.auto._
import io.circe.syntax._

import org.scalatest.WordSpec


class CommandRequestSpec extends WordSpec{


  "CommandRequest" must {

    "Work" in {
      implicit val actionIdFormatter = deriveEnumerationEncoder[SingleTargetAction]
      val request = SingleTargetActionRequest(UUID.randomUUID(), CrushingBlow)

      println(request.asJson)


    }

  }

}
