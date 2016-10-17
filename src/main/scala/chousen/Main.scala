package chousen

import scala.util.Random


object Main extends App {

  statement("Enter name: ")
  val name = scala.io.StdIn.readLine()
  val player = PlayerCharacter(name)
  val someEnemies: List[Set[BaseCharacter]] = List(/*Set(EnemyCharacter.slime()),*/
    Set(EnemyCharacter.yellowSlime(), EnemyCharacter.slime()),
    Set(EnemyCharacter.giantSlime()))

  GameLoop(name).loop(player, someEnemies)
}


case class State(playerAlive: Boolean, cast: Actors)

case class GameLoop(playerName: String) {
  story(s"$playerName has entered the dungeon")
  story(s"It was dark and smelly")
  statement(s"Eventually $playerName finds a room with a chest!")

  def loop(p: BaseCharacter, enemies: List[Set[BaseCharacter]]) = {

    // Need to place these elsewhere
    implicit val convert = (b: BaseCharacter) => Set(b)

    break()
    def innerLoop(actors: Actors): (Boolean, Actors) = {

      //FIXME
      def enemyAttack(): Actors = actors.actor.attack(actors.player, Option(actors.cast - actors.player))


      def playerAttack(): Actors = {
        val player = actors.player
        def turn(): Actors = {
          statement("[A]ttack [M]agic")
          val choice = scala.io.StdIn.readLine().toLowerCase

          choice match {
            case "a" => player.attack(actors.cast, None)
            case "m" => statement(s"$player does not know any magic"); turn()
            case _ => turn()
          }
        }

        statement(s"$player's turn")
        turn()
      }

      def postAttack(a: Actors): State = {
        val (alive: Set[BaseCharacter], dead: Set[BaseCharacter]) = a.cast.partition(cm => cm.currentHp > 0)
        dead.foreach(cm => exclaim(cm.deathMessage))
        val postActionCast = Actors(a.actor, alive)

        val playerAlive: Boolean = if (postActionCast.actor.isPlayer) true
        else postActionCast.cast
          .find(cm => cm.isPlayer)
          .map(pc => pc.currentHp > 0).get

        State(playerAlive, postActionCast)
      }

      // Move
      val cast = if (actors.actor.isPlayer) playerAttack() else enemyAttack()
      val eas = postAttack(cast)

      if (!eas.playerAlive) {
        (false, eas.cast)
      } else if (!eas.cast.hasEnemies) {
        (true, eas.cast)
      } else {
        innerLoop(eas.cast.changeTurn)
      }
    }

    def play(player: BaseCharacter, es: List[Set[BaseCharacter]]): List[Set[BaseCharacter]] = {
      if (es.nonEmpty) {
        // Get first enemy/enemies
        val e = es.head

        if (e.size == 1) exclaim(s"A ${e.head} appears")
        else {
          e.map(en => en.name)
          exclaim(s"${e.toString()} appears")
        }

        val act = Actors(player, e) // Implicit


        // Fight
        val result: (Boolean, Actors) = innerLoop(act)


        // Conclude
        if (result._1) play(result._2.player, es.tail)
        else es
      } else Nil
    }

    if (play(p, enemies).nonEmpty) {
      exclaim("Game over")
    } else {
      story(s"$playerName found a wooden chest")
      story(s"$playerName opens the wooden chest")
      break()
      suspense(s"Unfortunately")
      story(s" it was a trap and $playerName died")
    }
  }
}

object Engine {
  def calcDamage(a: BaseCharacter, d: BaseCharacter): Int = {
    val atkPwr = Dice.roll() + ((a.strength + a.dexterity) / 2)
    val defPwr: Int = d.vitality / 2
    atkPwr - defPwr
  }
}

object Dice {
  def roll(sides: Int = 6, min: Int = 1): Int = min + Random.nextInt(sides - 1)
}

case class Actors(actor: BaseCharacter, cast: Set[BaseCharacter]) {
  // Very simple turn change
  def changeTurn = {
    val next = cast.head

    Actors(next, cast + actor - next)
  }

  def player = cast.find(bc => bc.isPlayer).getOrElse(actor)

  def hasEnemies = !actor.isPlayer || cast.exists(!_.isPlayer)
}

trait Action {
  char: BaseCharacter =>

  def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors
}

trait Attack extends Action {
  char: BaseCharacter =>

  //TODO: Refactor
  def attack(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    if (target.size != 1) {
      if (char.isPlayer) {

        statement("Select a target:")

        val targets = target.foldLeft(Map.empty[Int, BaseCharacter]) {
          (m, bc: BaseCharacter) =>
            if (m.isEmpty) m + ((1, bc))
            else m.+((m.keySet.max + 1
              , bc))
        }.foldLeft(Map.empty[String, BaseCharacter])((m, bc) => m.+((bc._1.toString, bc._2)))

        val targetString = targets.map(kv => (kv._1, kv._2)).flatMap(kv => s"[${kv._1}]:${kv._2} ").mkString
        statement(targetString)

        val choice = scala.io.StdIn.readLine().toLowerCase

        targets.get(choice)
          .map(bc =>
            complete(Set(bc), Option(bystanders.getOrElse(Set()) ++ (target - bc))
            )).getOrElse(attack(target, bystanders))
      }
      else throw new RuntimeException("Enemies cannot attack multiple targets")
    } else {
      exclaim(s"$char attacks")

      complete(target, bystanders)
    }
  }

  override def complete(target: Set[BaseCharacter], bystanders: Option[Set[BaseCharacter]]): Actors = {
    val t = target.map { e =>
      val damage = Engine.calcDamage(char, e)
      if (isPlayer) exclaim(s"$char deals $damage to $e")
      e.takeDamage(damage)
    }
    Actors(char, t ++ bystanders.getOrElse(Set.empty[BaseCharacter]))
  }
}