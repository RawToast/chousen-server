package chousen

import scala.util.Random


object Main extends App {

  statement("Enter name: ")
  val name = scala.io.StdIn.readLine()
  val player = PlayerCharacter(name)
  val someEnemies: List[EnemyCharacter] = List(EnemyCharacter.slime(),
    EnemyCharacter.yellowSlime(),
    EnemyCharacter.giantSlime())

  GameLoop(name).loop(player, someEnemies)
}

case class GameLoop(playerName: String) {
  story(s"$playerName has entered the dungeon")
  story(s"It was dark and smelly")
  statement(s"Eventually $playerName finds a room with a chest!")

  def loop(p: BaseCharacter, enemies: List[BaseCharacter]) = {

    break()
    def innerLoop(player: BaseCharacter, enemy: BaseCharacter): (Boolean, BaseCharacter) = {

      def enemyAttack(): BaseCharacter = {
        enemy.attack(player)
      }

      def playerAttack(): BaseCharacter = {
        def turn(): BaseCharacter = {
          statement("[A]ttack [M]agic")
          val choice = scala.io.StdIn.readLine().toLowerCase

          choice match {
            case "a" => player.attack(enemy)
            case "m" => statement(s"${player.name} does not know any magic"); turn()
            case _ => turn()
          }
        }

        statement(s"${player.name}'s turn")
        turn()
      }

      // Move
      val enmy = playerAttack()
      val ply = if (enmy.currentHp > 0) enemyAttack() else player

      if (ply.currentHp <= 0) {
        exclaim(s"${ply.name} was killed by a ${enmy.name}")
        (false, ply)
      } else if (enmy.currentHp <= 0) {
        exclaim(s"${ply.name} killed a ${enmy.name}")
        (true, ply)
      } else innerLoop(ply, enmy)
    }

    def play(player:BaseCharacter, es:List[BaseCharacter]): List[BaseCharacter] = {
      if (es.nonEmpty) {
        val e = es.head
        exclaim(s"A ${e.name} appears")
        val result: (Boolean, BaseCharacter) = innerLoop(player, e)

        if (result._1) play(result._2, es.tail)
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

case class Actors(actor: BaseCharacter, cast: List[BaseCharacter])

trait Action {
  char: BaseCharacter =>


}


trait Attack {
  char: BaseCharacter =>
  def attack(enemy: BaseCharacter): BaseCharacter = {
    exclaim(s"${char.name} attacks")
    val damage = Engine.calcDamage(char, enemy)

    if (isPlayer) exclaim(s"${char.name} deals $damage to ${enemy.name}")
    enemy.takeDamage(damage)
  }
}
