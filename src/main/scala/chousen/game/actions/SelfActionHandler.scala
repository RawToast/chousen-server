package chousen.game.actions

import chousen.api.data.PlayerOptics._
import chousen.api.data._
import chousen.game.core.GameStateOptics.{MessagesLens, PlayerLens}
import chousen.util.LensUtil
import monocle.Lens


object SelfActionHandler {

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
    }


  def healWounds(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = {
    val healAmount = 10 + (2 + p.stats.intellect) + (p.stats.maxHp / 10)
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


  private def elixir(p: Player, msgs: Seq[GameMessage], stat: String, lens: Lens[Player, Int], amt: Int = 3) = {
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
    val bonusStat = 2
    val message = GameMessage(s"${p.name} looks at a Rare Pepe and becomes stronger!")
    val gameMessages = msgs :+ message

    val lens = LensUtil.triLens(PlayerMaxHealthLens, PlayerHealthLens, PlayerPositionLens)
      .modify { case (maxHp: Int, hp: Int, position: Int) => {
        val newMax = maxHp + 10
        (newMax, Math.min(newMax, hp + bonusStat), position - 100)
      }}.andThen(LensUtil.triLens(PlayerStrengthLens, PlayerDexterityLens, PlayerIntellectLens).modify {
      case (s: Int, d: Int, i: Int) => (s + 1, d + 1, i + 1)
    }).andThen(PlayerVitalityLens.modify(_ + 1))
    (lens.apply(p), gameMessages)
  }

  def quickStep(p: Player, msgs: Seq[GameMessage]): (Player, Seq[GameMessage]) = {
    val message = GameMessage(s"${p.name} uses Quick Step!")

    (PlayerPositionLens.modify(_ + 100 + p.stats.dexterity)(p), msgs :+ message)
  }
}
