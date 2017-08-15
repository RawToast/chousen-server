package chousen.game.actions

import chousen.api.data._
import chousen.Optics._
import chousen.game.status.{StatusBuilder, StatusCalculator}
import chousen.util.LensUtil
import monocle.Lens


class SelfActionHandler(sc: StatusCalculator) {

  def handle(action: SelfAction): (GameState) => GameState = {
    LensUtil.duoLens(PlayerLens, MessagesLens).modify{
      case (p:Player, msgs: Seq[GameMessage]) =>
        actions(action)(p, msgs)
    }
  }

  private def actions(actionId: SelfAction): (Player, Seq[GameMessage]) => (Player, Seq[GameMessage]) =
    actionId match {
      case HealWounds => healWounds
      case ElixirOfStrength => elixirOfStrength
      case ElixirOfDexterity => elixirOfDexterity
      case ElixirOfIntelligence => elixirOfIntellect
      case ElixirOfVitality => elixirOfVitality
      case RarePepe => rarePepe
      case QuickStep => quickStep
      case Haste => haste
      case PotionOfMight => might
      case PotionOfDexterity => dexterity
      case PotionOfIntelligence => intelligence
      case PotionOfStoneSkin => stoneskin
      case PotionOfRage => rage
      case PotionOfContinuation => continuation
      case PotionOfRegeneration => regeneration
    }


  def healWounds(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)

    val healAmount = 15 + (2 + sePlayer.stats.intellect) + (p.stats.maxHp / 10)
    val message = GameMessage(s"${p.name} uses Heal Wounds and recovers ${healAmount}HP!")
    val gameMessages = msgs :+ message

    val lens = LensUtil.duoLens(PlayerHealthLens, PlayerPositionLens)
      .modify{case (hp: Int, position: Int) =>
          Math.min(p.stats.maxHp, hp + healAmount) -> (position - 100)
         }
    (lens.apply(p), gameMessages)
  }

  def elixirOfStrength(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = elixir(p, msgs, "Strength", PlayerStrengthLens)

  def elixirOfDexterity(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = elixir(p, msgs, "Dexterity", PlayerDexterityLens)

  def elixirOfIntellect(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = elixir(p, msgs, "Intellect", PlayerIntellectLens)

  def elixirOfVitality(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = elixir(p, msgs, "Vitality", PlayerVitalityLens)


  private def elixir(p: Player, msgs: Seq[GameMessage], stat: String, lens: Lens[Player, Int], amt: Int = 2) = {
    val bonusStat = amt
    val message = GameMessage(s"${p.name} uses Elixir of $stat and gains $bonusStat $stat!")
    val gameMessages = msgs :+ message

    val effect = LensUtil.duoLens(lens, PlayerPositionLens)
      .modify { case (hp: Int, position: Int) =>
        Math.min(p.stats.maxHp, hp + bonusStat) -> (position - 100)
      }
    (effect.apply(p), gameMessages)
  }

  def rarePepe(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = {
    val cardMsg = GameMessage(s"${p.name} quickly looks at a Rare Pepe and becomes stronger!")

    val bonusExp = 3 + (p.experience.level * 5)
    val expMsg = GameMessage(s"${p.name} gains $bonusExp experience.")

    val lens = PlayerCurrentExperienceLens.modify(_ + 5)
      .andThen(PlayerPositionLens.modify(_ - 50))

    val gameMessages = msgs :+ cardMsg :+ expMsg

    (lens.apply(p), gameMessages)
  }

  def quickStep(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = {
    val sePlayer = sc.calculate(p)
    val message = GameMessage(s"${p.name} uses Quick Step!")

    (PlayerPositionLens.modify(_ + 100 + sePlayer.stats.dexterity)(p), msgs :+ message)
  }

  def haste(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} uses Haste!")

    val hasteStatus: Status = StatusBuilder.makeHaste(4)

    (PlayerStatusLens.modify(_ :+ hasteStatus)
//      .andThen(PlayerSpeedLens.modify(_ + 4))
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def might(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Might!")

    val hasteStatus: Status = StatusBuilder.makeMight(8)

    (PlayerStatusLens.modify(_ :+ hasteStatus)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def dexterity(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Dexterity!")

    val status: Status = StatusBuilder.makeDexterity(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def stoneskin(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Stone Skin!")

    val status: Status = StatusBuilder.makeStoneSkin(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def intelligence(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Intelligence!")

    val status: Status = StatusBuilder.makeSmart(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def rage(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Rage!")

    val status: Status = StatusBuilder.makeBerserk(4, turns = 8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def continuation(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Continuation!")

    (PlayerStatusLens.modify(_.map(s => s.copy(turns = s.turns + 5)))
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }

  def regeneration(p: Player, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Regeneration!")
    val sePlayer = sc.calculate(p)

    val hpFactor: Double = sePlayer.stats.maxHp / 20d
    val vitFactor: Double = sePlayer.stats.vitality / 8d

    val regenAmount:Int = {1 + hpFactor + vitFactor}.toInt


    val status: Status = StatusBuilder.makeRegen(regenAmount, turns = 8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), msgs :+ message)
  }
}
