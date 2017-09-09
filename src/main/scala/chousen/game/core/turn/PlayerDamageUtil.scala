package chousen.game.core.turn

import chousen.Optics
import chousen.Optics.EnemyStatusLens
import chousen.api.data._
import chousen.game.actions.DamageCalculator
import chousen.game.dungeon.EnemyBuilder

object PlayerDamageUtil {

  def doDamage(p: Player, enemies: Set[Enemy], messages: Seq[GameMessage], activeEnemy: Enemy)(implicit dc: DamageCalculator) = {

    val dmg = dc.calculateEnemyDamage(activeEnemy, p)
    lazy val noFortDmg = dc.calculateEnemyDamage(activeEnemy, p.copy(status = p.status.filterNot(_.effect == Fort)))

    val attackMessage = if (dmg < 5) GameMessage(s"${activeEnemy.name} grazes ${p.name} for $dmg damage.")
    else if (dmg < 10) GameMessage(s"${activeEnemy.name} hits ${p.name} for $dmg damage.")
    else if (dmg < 20) GameMessage(s"${activeEnemy.name} attacks ${p.name} for $dmg damage!")
    else if (dmg < 30) GameMessage(s"${activeEnemy.name} smashes ${p.name} for $dmg damage!!")
    else if (dmg < 40) GameMessage(s"${activeEnemy.name} crushes ${p.name} for $dmg damage!!!")
    else if (dmg < 50) GameMessage(s"${activeEnemy.name} bops ${p.name} for $dmg damage!!!")
    else GameMessage(s"${activeEnemy.name} crits ${p.name} for $dmg damage!!!!!")

    // Then reset
    val es = finish(enemies, activeEnemy)

    val isTripleOrc = es.map(_.name).contains("TripleOrc") & es.size < 4
    val finalEs = if (isTripleOrc) es + EnemyBuilder.smallOrc else es

    val msg2 = if (isTripleOrc) Option(GameMessage(s"A Tiny Orc comes to the aid of TripleOrc")) else None

    val damagedPlayer = takeDamage(p, dmg)

    val reduceFortPlayer = Optics.PlayerStatusLens.modify(_.map(sf =>
      if (sf.effect == Fort) sf.copy(amount = sf.amount.map(_ - (noFortDmg - dmg)))
      else sf)).andThen(Optics.PlayerStatusLens.modify(_.filter(sf =>
      (sf.effect != Fort) || sf.amount.getOrElse(0) > 0
    )))(damagedPlayer)

    (reduceFortPlayer, finalEs, (messages :+ attackMessage) ++ msg2)
  }


  def finish(es: Set[Enemy], ae: Enemy, st: Option[Status]=None) = {
    def affectStatus = EnemyStatusLens.modify(handleStatus)
    def handleStatus(status: Seq[Status]): Seq[Status] = {
      val altStatus = status.filter(_.turns > 0).filter(s => s.effect == Poison)
      val mainStatus= status.filter(_.turns > 0).filterNot(s => s.effect == Poison)

      altStatus ++
        mainStatus.map(s => s.copy(turns = s.turns - 1))
    }

    def reset(e: Enemy) = e.copy(position = e.position - 100)


    // Then reset
    import chousen.Implicits._
    es.map(e => if (e ~= ae) affectStatus.andThen(reset).andThen(EnemyStatusLens.modify(s => s ++ st.toSeq))(e) else e)
  }

  private def takeDamage(p: Player, dmg: Int) = PlayerOptics.PlayerCharStatsLens
    .composeLens(CharStatsOptics.HpLens).modify(hp => hp - dmg)(p)
}
