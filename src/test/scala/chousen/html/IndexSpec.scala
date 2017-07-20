package chousen.html

import org.scalatest.WordSpec
import play.twirl.api.Html


class IndexSpec extends WordSpec {

  "Index.html" should {


    val indexPage: Html = chousen.html.index("dummyVersion", "dummyDate")

    "Include a box for the user to input their name" in {

      assert(indexPage.body contains "<input id =\"namebox\" type=\"text\"")

    }

  }

}
