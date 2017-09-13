package chousen.game.actions

import chousen.api.data._
import chousen.Optics._
import chousen.game.status.{StatusBuilder, StatusCalculator}
import chousen.util.LensUtil
import monocle.Lens
import chousen.game.core.turn.PositionCalculator.{FAST, calculatePosition}


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
      case Barrier => barrier

      case ElixirOfStrength => elixirOfStrength
      case ElixirOfDexterity => elixirOfDexterity
      case ElixirOfIntelligence => elixirOfIntellect
      case ElixirOfVitality => elixirOfVitality
      case RarePepe => rarePepe

      case QuickStep => quickStep
      case Haste => haste

      case FortifyArmour => fortify

      case PotionOfMight => might
      case PotionOfDexterity => dexterity
      case PotionOfIntelligence => intelligence
      case PotionOfStoneSkin => stoneskin
      case PotionOfRage => rage
      case PotionOfTrogg => trogg
      case PotionOfContinuation => continuation
      case PotionOfRegeneration => regeneration
      case PotionOfLignification => lignification

      case EssenceOfStrength => essenceOfStrength
      case EssenceOfDexterity => essenceOfDexterity
      case EssenceOfVitality => essenceOfVitality
      case EssenceOfIntelligence => essenceOfIntellect
    }

  type Update = (Player, Cards, Seq[GameMessage])


  def healWounds(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val sePlayer = sc.calculate(p)

    val healAmount = 15 + (2 + sePlayer.stats.intellect) + (p.stats.maxHp / 10) + sePlayer.experience.level
    val message = GameMessage(s"${p.name} uses Heal Wounds and recovers ${healAmount}HP!")
    val gameMessages = msgs :+ message

    val lens = LensUtil.duoLens(PlayerHealthLens, PlayerPositionLens)
      .modify { case (hp: Int, position: Int) =>
        Math.min(p.stats.maxHp, hp + healAmount) -> (position - 100)
      }
    (lens.apply(p), cs, gameMessages)
  }

  def barrier(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val sePlayer = sc.calculate(p)

    val message = GameMessage(s"${p.name} uses Barrier!")
    val gameMessages = msgs :+ message

    val blockStatus = StatusBuilder.makeBlock(turns = sePlayer.stats.intellect / 5)


    val pWithPos = calculatePosition(p)

    val lens = PlayerStatusLens.modify(_ :+ blockStatus)
      (lens.apply(pWithPos), cs, gameMessages)
  }

  def fortify(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val message = GameMessage(s"${p.name} uses his gold to fortify his armour!")
    val gameMessages = msgs :+ message
    val seInt = sc.calculate(p).stats.intellect

    val blockStatus = StatusBuilder.makeFort(
      turns = 1 + (seInt / 3),
      amount = 20 + (p.experience.level * 10) + (seInt * 2))

    val pWithPos = calculatePosition(p)

    val lens = PlayerStatusLens.modify(_ :+ blockStatus)
    (lens.apply(pWithPos), cs, gameMessages)
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

    val effect = lens.modify(x => x + bonusStat).andThen(calculatePosition(_ :Player))

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
      .andThen(calculatePosition(_ :Player, cost = FAST))

    val gameMessages = msgs :+ cardMsg :+ expMsg

    (lens.apply(p), cs, gameMessages)
  }

  def quickStep(p: Player, cs: Cards, msgs: Seq[GameMessage]): Update = {
    val sePlayer = sc.calculate(p)
    val message = GameMessage(s"${p.name} uses Quick Step!")

    (PlayerPositionLens.modify(_ + 50 + (sePlayer.stats.dexterity * 2))(p), cs, msgs :+ message)
  }

  def haste(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} uses Haste!")

    val hasteStatus: Status = StatusBuilder.makeHaste(4)

    (PlayerStatusLens.modify(_ :+ hasteStatus)
      //      .andThen(PlayerSpeedLens.modify(_ + 4))
      .andThen(calculatePosition(_ :Player, cost = FAST))(p), cs, msgs :+ message)
  }

  def might(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Might!")

    val hasteStatus: Status = StatusBuilder.makeMight(8)

    (PlayerStatusLens.modify(_ :+ hasteStatus)
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  def dexterity(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Dexterity!")

    val status: Status = StatusBuilder.makeDexterity(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  def stoneskin(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Stone Skin!")

    val status: Status = StatusBuilder.makeStoneSkin(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  def lignification(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Lignification!")

    val status: Status = StatusBuilder.makeTree(4)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  def intelligence(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Intelligence!")

    val status: Status = StatusBuilder.makeSmart(8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  def rage(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Rage!")

    val status: Status = StatusBuilder.makeBerserk(4, turns = 8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  def trogg(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Trogg!")
    val ragePots = cs.hand.count(_.action == PotionOfRage)

    val newHand = cs.hand.filterNot(_.action == PotionOfRage)
    val toDiscard = cs.hand.filter(_.action == PotionOfRage)

    val cards = cs.copy(hand = newHand, discard = cs.discard ++ toDiscard)
    val status: Status = StatusBuilder.makeBerserk(6 + (ragePots * 2), turns = 8)

    (PlayerStatusLens.modify(_ :+ status)
      .andThen(potionPositionCalc)(p), cards, msgs :+ message)
  }

  def continuation(p: Player, cs: Cards, msgs: Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} drinks a Potion of Continuation!")

    (PlayerStatusLens.modify(_.map(s => s.copy(turns = s.turns + 6)))
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
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
      .andThen(potionPositionCalc)(p), cs, msgs :+ message)
  }

  private val potionPositionCalc =  calculatePosition(_ :Player, cost = FAST)
}
