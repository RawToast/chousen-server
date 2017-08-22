package chousen.game.actions

import chousen.api.data._
import chousen.Optics._
import chousen.game.status.{StatusBuilder, StatusCalculator}
import chousen.util.LensUtil
import monocle.Lens


class SelfActionHandler(sc: StatusCalculator) extends ActionHandler {

  def handle(action: SelfAction): (GameState) => GameState = {
    LensUtil.triLens(PlayerLens, CardsLens, MessagesLens).modify {
      case (p: Player, cs: Cards, msgs: Seq[GameMessage]) =>
        actions(action)(p, cs, msgs)
    }.andThen(handleDead)
  }

  private def actions(actionId: SelfAction): (Player, Cards, Seq[GameMessage]) => (Player, Cards, Seq[GameMessage]) =
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
      case EssenceOfStrength => essenceOfStrength
      case EssenceOfDexterity => essenceOfDexterity
      case EssenceOfVitality => essenceOfVitality
      case EssenceOfIntelligence => essenceOfIntellect
    }

  type Update = (Player, Cards, Seq[GameMessage])


  def healWounds(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val sePlayer = sc.calculate(p)

    val healAmount = 15 + (2 + sePlayer.stats.intellect) + (p.stats.maxHp / 10)
    val message = GameMessage(s"${p.name} uses Heal Wounds and recovers ${healAmount}HP!")
    val gameMessages = msgs :+ message

    val lens = LensUtil.duoLens(PlayerHealthLens, PlayerPositionLens)
      .modify { case (hp: Int, position: Int) =>
        Math.min(p.stats.maxHp, hp + healAmount) -> (position - 100)
      }
    (lens.apply(p), cs, gameMessages)
  }

  def elixirOfStrength(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update =
    elixir(p, cs, msgs, "Strength", PlayerStrengthLens)

  def elixirOfDexterity(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update =
    elixir(p, cs, msgs, "Dexterity", PlayerDexterityLens)

  def elixirOfIntellect(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update =
    elixir(p, cs, msgs, "Intellect", PlayerIntellectLens)

  def elixirOfVitality(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update =
    elixir(p, cs, msgs, "Vitality", PlayerVitalityLens)


  private def elixir(p: Player, cs: Cards, msgs: Seq[GameMessage], stat: String, lens: Lens[Player, Int], amt: Int = 2) = {
    val bonusStat = amt
    val message = GameMessage(s"${p.name} uses Elixir of $stat and gains $bonusStat $stat!")
    val gameMessages = msgs :+ message

    val effect = LensUtil.duoLens(lens, PlayerPositionLens)
      .modify { case (hp: Int, position: Int) =>
        hp + bonusStat -> (position - 100)
      }
    (effect.apply(p), cs, gameMessages)
  }

  def essenceOfStrength(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    essence(p, cs, msgs, "Strength", PlayerStrengthLens)
  }

  def essenceOfDexterity(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    essence(p, cs, msgs, "Dexterity", PlayerDexterityLens)
  }

  def essenceOfIntellect(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    essence(p, cs, msgs, "Intellect", PlayerIntellectLens)
  }

  def essenceOfVitality(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    essence(p, cs, msgs, "Vitality", PlayerVitalityLens)
  }

  private def essence(p: Player, cs: Cards, msgs: Seq[GameMessage], stat: String, lens: Lens[Player, Int]) = {
    if (cs.playedEssence) (p, cs, msgs)
    else {
      val bonusStat = 1
      val message = GameMessage(s"${p.name} uses Essence of $stat and gains $bonusStat $stat!")
      val gameMessages = msgs :+ message

      val effect = lens.modify { x =>
          Math.min(p.stats.maxHp, x + bonusStat)
        }
      (effect.apply(p), cs.copy(playedEssence = true), gameMessages)
    }
  }

  def rarePepe(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val cardMsg = GameMessage(s"${p.name} quickly looks at a Rare Pepe and becomes stronger!")

    val bonusExp = 3 + (p.experience.level * 5)
    val expMsg = GameMessage(s"${p.name} gains $bonusExp experience.")

    val lens = PlayerCurrentExperienceLens.modify(_ + 5)
      .andThen(PlayerPositionLens.modify(_ - 50))

    val gameMessages = msgs :+ cardMsg :+ expMsg

    (lens.apply(p), cs, gameMessages)
  }

  def quickStep(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val sePlayer = sc.calculate(p)
    val message = GameMessage(s"${p.name} uses Quick Step!")

    (PlayerPositionLens.modify(_ + 100 + sePlayer.stats.dexterity)(p), cs, msgs :+ message)
  }

  def haste(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} uses Haste!")

    val hasteStatus: Status = StatusBuilder.makeHaste(4)

    (PlayerStatusLens.modify(_ :+ hasteStatus)
      //      .andThen(PlayerSpeedLens.modify(_ + 4))
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def might(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Might!")

    val hasteStatus: Status = StatusBuilder.makeMight(8)

    (PlayerStatusLens.modify(_ :+ hasteStatus)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def dexterity(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Dexterity!")

    val status: Status = StatusBuilder.makeDexterity(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def stoneskin(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Stone Skin!")

    val status: Status = StatusBuilder.makeStoneSkin(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def intelligence(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Intelligence!")

    val status: Status = StatusBuilder.makeSmart(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def rage(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Rage!")

    val status: Status = StatusBuilder.makeBerserk(4, turns = 8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def continuation(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Continuation!")

    (PlayerStatusLens.modify(_.map(s => s.copy(turns = s.turns + 5)))
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }

  def regeneration(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Regeneration!")
    val sePlayer = sc.calculate(p)

    val hpFactor: Double = sePlayer.stats.maxHp / 20d
    val vitFactor: Double = sePlayer.stats.vitality / 8d

    val regenAmount: Int = {
      1 + hpFactor + vitFactor
    }.toInt


    val status: Status = StatusBuilder.makeRegen(regenAmount, turns = 8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(PlayerPositionLens.modify(i => i - 50))(p), cs, msgs :+ message)
  }
}
