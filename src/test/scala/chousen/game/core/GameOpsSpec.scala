package chousen.game.core

import java.util.UUID

import chousen.api.data.{CharStats, Enemy, GameMessage, Player}
import org.scalatest.WordSpec

class GameOpsSpec extends WordSpec {

  def speed10Char = CharStats(100, 100, speed = 10)

  def speed8Char = CharStats(100, 100, speed = 8)

  def resetActive(t:(Player, Set[Enemy], Seq[GameMessage])): (Player, Set[Enemy]) = {
    import chousen.api.types.Implicits._
    val (p, es, _) = t

    val enemy = es.maxBy(_.position)

    if (enemy.position > p.position) {
      val re = enemy.copy(position = enemy.position - 100)
      p -> es.map(e => if (e ~= re) re else e)
    }
    else p.copy(position = p.position - 100) -> es
  }

  "GameOps.update" when {

    "provided with a fast player and a slow enemy" should {
      val player = Player("Player", speed10Char, position = 0)
      val enemy = Enemy("Enemy", UUID.randomUUID(), speed8Char, position = 0)
      val emptyMessages = Seq.empty[GameMessage]

      val (updPlayer, updEnemies, updMessages) = GameOps.update(player, Set(enemy), emptyMessages)

      "Set the fast player to be the active character" in {
        assert(!updEnemies.exists(e => e.position > updPlayer.position))
      }

      "Set the slow enemy's position to 80" in {
        assert(updEnemies.head.position == 80)
      }

      "Set the fast player's position to 100" in {
        assert(updPlayer.position >= 100)
      }

      "Includes a game message stating it's the fast player's turn" in {
        assert(updMessages.size == 1)
        assert(updMessages.head.text == "Player's turn!")
      }

    }


    "provided two a player and enemy with equal speeds and different positions" should {

      // Note that the enemy is closer to their next turn
      val player = Player("Player", speed10Char, position = 0)
      val enemy = Enemy("Quick Enemy", UUID.randomUUID(), speed10Char, position = 50)
      val emptyMessages = Seq.empty[GameMessage]

      val (updPlayer, updEnemies, updMessages) =
        GameOps.update(player, Set(enemy), emptyMessages)

      "Set the highest positioned player to be the actor with a higher position" in {
        assert(updEnemies.maxBy(_.position).position > updPlayer.position)
      }

      "Set the lower position character's position to 50" in {
        assert(updPlayer.position == 50)
      }

      "Set the higher position character's position to 100" in {
        assert(updEnemies.exists(_.position == 100))
      }

      "Include a game message stating it's the higher positioned users turn" in {
        assert(updMessages.size == 1)
        assert(updMessages.head.text == "Quick Enemy's turn!")
      }

      "updating again" must {

        val (latestPlayer, latestEnemies, latestMessages) =
          GameOps.update(updPlayer, updEnemies, updMessages)

        "retain the active actor" in {
          assert(updEnemies.maxBy(_.position).position > updPlayer.position)
          assert(latestEnemies.maxBy(_.position).position > latestPlayer.position)
        }

        "Include an additional message with the same text" in {
          assert(latestMessages.size != updEnemies.size)
          assert(latestMessages.size == 2)
          assert(latestMessages.last.text == "Quick Enemy's turn!")

          // Same message
          assert(latestMessages.toSet.size == 1)
        }

      }

      "updating after the enemy has been reset" must {

        val es = updEnemies.map(e => e.copy(position = e.position - 100))
        val (latestPlayer, latestEnemies, latestMessages) =
          GameOps.update(updPlayer, es,
            updMessages)

        "change the active actor" in {
          assert(updEnemies.maxBy(_.position).position > updPlayer.position)
          assert(latestEnemies.maxBy(_.position).position < latestPlayer.position)
        }

        "Include a game message stating it's now the Player's turn" in {
          assert(latestMessages.size == 2)
          assert(latestMessages.exists(_.text == "Player's turn!"))
          assert(latestMessages.last.text == "Player's turn!")
        }

      }


      "two actors reach the position goal" should {

        // Note that the enemy is closer to their next turn
        val player = Player("Player", speed10Char, position = 90)
        val enemy = Enemy("Enemy", UUID.randomUUID(), speed10Char, position = 91)
        val emptyMessages = Seq.empty[GameMessage]

        val (updPlayer, updEnemies, _) =
          GameOps.update(player, Set(enemy), emptyMessages)

        "the faster actor's position is >= 100" in {
          assert(updEnemies.head.position == 101)
        }

        "the other character's position is >= 100" in {
          assert(updPlayer.position == 100)
        }

        "the actor with the highest position takes precedence" in {
          assert(updEnemies.maxBy(_.position).position >= updPlayer.position)
        }

      }
    }

    "provided with an equal enemy and player" should {

      val player = Player("Player", speed10Char, position = 0)
      val enemy = Enemy("Quick Enemy", UUID.randomUUID(), speed10Char, position = 0)

      val emptyMessages = Seq.empty[GameMessage]
      val turnOneCast = GameOps.update(player, Set(enemy), emptyMessages)

      val (t1Player, t1Enemies) = resetActive(turnOneCast)
      val turnTwoCast = GameOps.update(t1Player, t1Enemies, turnOneCast._3)

      val (t2Player, t2Enemies) = resetActive(turnTwoCast)
      val turnThreeCast = GameOps.update(t2Player, t2Enemies, turnTwoCast._3)

      val (t3Player, t3Enemies) = resetActive(turnThreeCast)
      val turnFourCast = GameOps.update(t3Player, t3Enemies, turnThreeCast._3)

      def nameOfActive(t: (Player, Set[Enemy], Seq[GameMessage])): String = {
        val (player, e, _) = t

        val enemy = e.maxBy(_.position)

        if (enemy.position > player.position) enemy.name
        else player.name

      }

      "have a different actor on the 1st/2nd turn" in assert {
        nameOfActive(turnOneCast) != nameOfActive(turnTwoCast)
      }

      "have a different actor on the 3rd/4th turn" in assert {
        nameOfActive(turnThreeCast) != nameOfActive(turnFourCast)
      }

      "have a different actor on each following turn" in {

        Range.inclusive(0, 5).foreach { (_: Int) =>

          val player = Player("Player", speed10Char, position = 0)
          val enemy = Enemy("Quick Enemy", UUID.randomUUID(), speed10Char, position = 0)
          val emptyMessages = Seq.empty[GameMessage]

          val turnOneCast = GameOps.update(player, Set(enemy), emptyMessages)

          val (t1Player, t1Enemies) = resetActive(turnOneCast)
          val turnTwoCast = GameOps.update(t1Player, t1Enemies, turnOneCast._3)

          val (t2Player, t2Enemies) = resetActive(turnTwoCast)
          val turnThreeCast = GameOps.update(t2Player, t2Enemies, turnTwoCast._3)

          val (t3Player, t3Enemies) = resetActive(turnThreeCast)
          val turnFourCast = GameOps.update(t3Player, t3Enemies, turnThreeCast._3)

          assert(nameOfActive(turnOneCast) != nameOfActive(turnTwoCast))
          assert(nameOfActive(turnThreeCast) != nameOfActive(turnFourCast))
        }

      }
    }
  }

  //TODO in progress
  "GameOps.updateUntilPlayerIsActive" when {
    "provided with an ahead equal enemy and player" should {

      val player = Player("Player", speed10Char, position = 0)
      val enemy = Enemy("Quick Enemy", UUID.randomUUID(), speed10Char, position = 50)
      val emptyMessages = Seq.empty[GameMessage]

      val next = GameOps.updateUntilPlayerIsActive(player, Set(enemy), emptyMessages)
      val (nextPlayer, _, nextMessages) = next


      "create three messages" in {
        assert(nextMessages.nonEmpty)
        assert(next._3.size == 4)
      }

      "include message a for the enemy becoming active" in {
        assert(nextMessages.size > 1)
        assert(nextMessages.head.text == "Quick Enemy's turn!")
      }

      "include a message for the enemies actions" in {
        val attackText = "Quick Enemy attacks Player!"
        val damageText = "Quick Enemy deals "
        def messageIndex(m: String) = nextMessages.indexWhere(p => p.text.contains(m))

        assert(nextMessages.exists(gm => gm.text.contains(attackText)))
        assert(nextMessages.exists(gm => gm.text.contains(damageText)))

        // Ensure order
        assert(messageIndex(attackText) < messageIndex(damageText))
      }

      "the last message states the player is active" in {
        assert(nextMessages.last.text == "Player's turn!")
      }

      "the player's current hp is reduced" in {
        assert(nextPlayer.stats.maxHp > nextPlayer.stats.currentHp)

        // Additional checks
        assert(player.stats.maxHp == nextPlayer.stats.maxHp)
        assert(player.stats.currentHp > nextPlayer.stats.currentHp)
      }

      "result in the player being active" in {
        assert(EncounterOps.getActive(next) == Left(nextPlayer))
      }
    }
  }
}
