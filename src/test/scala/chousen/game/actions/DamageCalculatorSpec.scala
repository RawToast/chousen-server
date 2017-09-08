package chousen.game.actions

import java.util.UUID

import chousen.api.data._
import chousen.Optics._
import chousen.game.status.{StatusBuilder, StatusCalculator}
import org.scalatest.WordSpec

class DamageCalculatorSpec extends WordSpec {

  "DamageCalculator" should {

    lazy val statusCalculator = new StatusCalculator()
    lazy val damageCalculator = new DamageCalculator(statusCalculator)

    lazy val player = Player("test", "class", CharStats(10, 10), Experience(), Equipment(), 0, 0)
    lazy val enemy = GameStateGenerator.firstEnemy


    "Calculate damage" when {
      "A Player attacks an enemy" must {

        lazy val dmg = damageCalculator.calculatePlayerDamage(player, enemy)

        "Return a number greater than 0" in {
          assert(dmg > 0)
        }

        "Deal more damage if the Player has the Might status" in {
          val mightyPlayer = PlayerStatusLens.modify(_ :+ StatusBuilder.makeMight(5))(player)
          val mightyDmg = damageCalculator.calculatePlayerDamage(mightyPlayer, enemy)

          assert(mightyDmg > dmg)
        }

        "Deal more damage if the Player has the Rage status" in {
          val ragePlayer = PlayerStatusLens.modify(_ :+ StatusBuilder.makeBerserk(5))(player)
          val rageDmg = damageCalculator.calculatePlayerDamage(ragePlayer, enemy)

          assert(rageDmg > dmg)
        }

        "Deal more damage if the Player has a weapon with the Magic Status effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Magic)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg > dmg)
        }

        "Deal more damage if the Player has a weapon with the Crush Status effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Crush)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg > dmg)
        }

        "Deal more damage if the Player has a weapon with the Maim Status effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Maim)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg > dmg)
        }

        "Deal more damage if the Player has a weapon with the Quick Status effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Quick)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg > dmg)
        }

        "Deal more damage if the Player has a weapon with the Deceive Status effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Deceive)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg > dmg)
        }

        "Deal the same damage if the Player has a weapon with the Toxic effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Toxic)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg == dmg)
        }

        "Deal the same damage if the Player has a weapon with the Protection effect" in {
          val magicDmgWeaponPlayer = giveWeaponWithStatus(player, Protection)
          val magicDmg = damageCalculator.calculatePlayerDamage(magicDmgWeaponPlayer, enemy)

          assert(magicDmg == dmg)
        }

        "Deal less damage if the enemy has the StoneSkin status" in {
          val stoneskinEnemy = EnemyStatusLens.set(Seq(StatusBuilder.makeStoneSkin(5)))(enemy)
          val magicDmg = damageCalculator.calculatePlayerDamage(player, stoneskinEnemy)

          assert(magicDmg < dmg)
        }
      }

      "An enemy attacks the player" must {

        lazy val dmg = damageCalculator.calculateEnemyDamage(enemy, player)

        "Return a number greater than 0" in {
          assert(dmg > 0)
        }

        "Deal the same damage if the player has a weapon" in {
          val playedWithWeapon = giveWeaponWithStatus(player, Quick)
          val magicDmg = damageCalculator.calculateEnemyDamage(enemy, playedWithWeapon)

          assert(magicDmg == dmg)
        }

        "Deal less damage if the player has a Protection weapon" in {
          val protectWeaponPlayer = giveWeaponWithStatus(player, Protection)
          val magicDmg = damageCalculator.calculateEnemyDamage(enemy, protectWeaponPlayer)

          assert(magicDmg < dmg)
        }
      }
    }

    def giveWeaponWithStatus(p: Player, we: WeaponEffect) =
      PlayerWeaponLens.set(Some(Weapon(UUID.randomUUID(), "Weapon", 0, Requirements(), Seq(we))))(p)
  }

}
