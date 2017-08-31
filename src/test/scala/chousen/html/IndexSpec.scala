package chousen.html

import org.scalatest.WordSpec
import play.twirl.api.Html


class IndexSpec extends WordSpec {

  "Index.html" should {


    val indexPage: Html = chousen.ui.html.index()

    "Include a box for the user to input their name" in {

      assert(indexPage.body contains "<input id =\"namebox\" type=\"text\"")

    }

  }

}
