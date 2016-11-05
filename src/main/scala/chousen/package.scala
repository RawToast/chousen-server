import chousen.character.BaseCharacter

/**
  * Created by jim on 05/10/2016.
  */
package object chousen {

  def charToListChar: (BaseCharacter) => List[BaseCharacter] = (char:BaseCharacter) => List(char)

  def statement(string: String) = printer(string, None)
  def break() = printer("", Option("..."), newLine = true)


  def story(string: String) = printer(string, Option("..."))
  def suspense(string: String) = printer(string, Option("..."), newLine = false)

  def enemy(string:String) = printer(string, Option("!"))
  def exclaim(string:String) = printer(string, Option("!"))

  def requirePlayerInput = PlayerInput.apply()

  type UserInput = () => String

  def PlayerInput: UserInput = scala.io.StdIn.readLine().toLowerCase

  def requireCaseSensitivePlayerInput = scala.io.StdIn.readLine()


  private def printer(string: String, break:Option[String]=None, newLine:Boolean=true) {

    def space() { Thread.sleep(15); print(" ") }

    def sPrint(c: Char, sleep:Int=35) { Thread.sleep(sleep); print(c) }

    def printString: (String) => Unit = {
      str => str.foreach(c => {
        sPrint(c)
      })
    }

    val words = string.split(" ")

    printString(words.head)

    words.tail.foreach { w =>
      space
      printString(w)
    }

    break.foreach(_.foreach(c => sPrint(c, sleep = 140)))
    if (newLine)
      println("")
  }
}
