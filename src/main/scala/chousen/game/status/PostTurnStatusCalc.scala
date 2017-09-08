package chousen.game.status

import chousen.Optics.{MessagesLens, PlayerHealthLens, PlayerLens, PlayerStatusLens, CurrentEnemiesLens}
import chousen.api.data._
import chousen.util.LensUtil


class PostTurnStatusCalculator extends PostTurnStatusCalc

trait PostTurnStatusCalc {

  def applyStatusEffects(game: GameState, action: Action): GameState = {
    val ReducePlayerStatuses: (GameState) => GameState = reducePlayerStatuses(_, action)
    val ReduceEnemyStatuses: (GameState) => GameState = reduceEnemyStatuses(_, action)

    def playerStatusCalc(pm: (Player, Seq[GameMessage])): (Player, Seq[GameMessage]) =
      playerStatusEffects(pm._1, pm._2, action)

    def enemyStatusCalc(pm: (Set[Enemy], Seq[GameMessage])): (Set[Enemy], Seq[GameMessage]) =
      enemyStatusEffects(pm._1, pm._2, action)

    // Apply any Player effecting status effects:
    val ApplyPlayerStatusEffects: (GameState) => GameState =
      LensUtil.duoLens(PlayerLens, MessagesLens)
        .modify(playerStatusCalc)
        .andThen(ReducePlayerStatuses)

    val ApplyEnemyStatusEffects: (GameState) => GameState =
      LensUtil.duoLens(CurrentEnemiesLens, MessagesLens)
        .modify(enemyStatusCalc)
        .andThen(ReduceEnemyStatuses)


    ApplyPlayerStatusEffects.andThen(ApplyEnemyStatusEffects)(game)
  }


  private def enemyStatusEffects(es: Set[Enemy], msgs: Seq[GameMessage], action: Action) = {


    action match {
      case _: CardAction => (es, msgs)
      case _: CampFireAction => (es, msgs)
      case (a: SelfAction)
        if(a == EssenceOfStrength || a == EssenceOfVitality || a == EssenceOfDexterity || a == EssenceOfIntelligence)
          => (es, msgs)
      case _ =>

        import cats.instances.option.catsKernelStdMonoidForOption
        import cats.instances.int.catsKernelStdGroupForInt
        import cats.implicits.catsSyntaxSemigroup

        val newEs: Set[(Enemy, Seq[GameMessage])] = es.map(e => {
          val regenEffects = e.status
            .map(s => if (s.effect == Tree) s.copy(effect = Regen) else s)
            .filter(_.effect == Regen)
            .reduceLeftOption[Status] { case (a, b) => a.copy(amount = a.amount |+| b.amount) }

          val poisEffects = e.status.filter(s => s.effect == Poison)
            .reduceLeftOption[Status] { case (a, b) => a.copy(amount = a.amount |+| b.amount) }


          val effectsForComputation: Seq[Status] = e.status
            .filterNot(s => s.effect == Regen || s.effect == Poison) ++ regenEffects ++ poisEffects

          effectsForComputation.foldLeft(e -> Seq.empty[GameMessage]) {
            case (pm: (Enemy, Seq[GameMessage]), st: Status) =>
              val e = pm._1
              val m = pm._2

              st.effect match {
                case Regen =>
                  val regenAmt = st.amount.getOrElse(0)
                  val regenedEnemy = EnemyOptics.EnemyHpLens.modify(hp => Math.min(e.stats.maxHp, hp + regenAmt))(e)
                  val heal = regenedEnemy.stats.currentHp - e.stats.currentHp
                  if (heal > 0) regenedEnemy -> (m :+ GameMessage(s"${e.name} regenerates $heal health"))
                  else regenedEnemy -> m
                case Poison =>
                  val poisAmt = st.amount.getOrElse(0)
                  val resultEnemy = EnemyOptics.EnemyHpLens.modify(hp => hp - poisAmt)(e)
                  val dmg = -1 * (resultEnemy.stats.currentHp - e.stats.currentHp)
                  resultEnemy -> (m :+ GameMessage(s"${e.name} is poisoned and loses $dmg health"))
                case _ => pm
              }
          }
        })

        newEs.map(_._1) -> newEs.map(_._2).fold(msgs)(_ ++ _)
    }
  }



  private def playerStatusEffects(p: Player, msgs: Seq[GameMessage], action: Action) = {
    action match {
      case _: CardAction => (p, msgs)
      case _: CampFireAction => (p, msgs)
      case (a: SelfAction)
        if a == EssenceOfStrength || a == EssenceOfVitality || a == EssenceOfDexterity || a == EssenceOfIntelligence
          => (p, msgs)
      case _ =>

        import cats.instances.option.catsKernelStdMonoidForOption
        import cats.instances.int.catsKernelStdGroupForInt
        import cats.implicits.catsSyntaxSemigroup


        val regenEffects = p.status
          .map(s => if (s.effect == Tree) s.copy(effect = Regen) else s)
          .filter(_.effect == Regen)
          .reduceLeftOption[Status] { case (a, b) => a.copy(amount = a.amount |+| b.amount) }


        val effectsForComputation: Seq[Status] = p.status.filterNot(_.effect == Regen) ++ regenEffects

        effectsForComputation.foldLeft(p -> msgs) {
          case (pm: (Player, Seq[GameMessage]), st: Status) =>
            val p = pm._1
            val m = pm._2

            st.effect match {
              case Regen =>
                val regenAmt = st.amount.getOrElse(0)
                val regendPlayer = PlayerHealthLens.modify(hp => Math.min(p.stats.maxHp, hp + regenAmt))(p)
                val heal = regendPlayer.stats.currentHp - p.stats.currentHp
                if (heal > 0) regendPlayer -> (m :+ GameMessage(s"${p.name} regenerates $heal health"))
                else regendPlayer -> m
              case Poison =>
                val poisAmt = st.amount.getOrElse(0)
                val regendPlayer = PlayerHealthLens.modify(hp => hp - poisAmt)(p)
                val heal = -1 * (regendPlayer.stats.currentHp - p.stats.currentHp)
                regendPlayer -> (m :+ GameMessage(s"${p.name} is poisoned and loses $heal health"))
              case _ => pm
            }
        }
    }
  }




  private def reducePlayerStatuses(gs: GameState, a: Action) =
    a match {
      case _: SingleTargetAction => PlayerLens.composeLens(PlayerStatusLens).modify(reduceStatusLength)(gs)
      case _: MultiAction => PlayerLens.composeLens(PlayerStatusLens).modify(reduceStatusLength)(gs)
      case s: SelfAction => s match {
        case EssenceOfStrength => gs
        case EssenceOfDexterity => gs
        case EssenceOfIntelligence => gs
        case EssenceOfVitality => gs
        case _ => PlayerLens.composeLens(PlayerStatusLens).modify(reduceStatusLength)(gs)
      }
      case _: EquipAction => PlayerLens.composeLens(PlayerStatusLens).modify(reduceStatusLength)(gs)
      case _: CardAction => gs
      case _: CampFireAction => gs
    }

  private def reduceEnemyStatuses(gs: GameState, a: Action) =
    a match {
      case _: SingleTargetAction => CurrentEnemiesLens.modify(_.map(reduceEnemyStatusLength))(gs)
      case _: MultiAction => CurrentEnemiesLens.modify(_.map(reduceEnemyStatusLength))(gs)
      case _: SelfAction => CurrentEnemiesLens.modify(_.map(reduceEnemyStatusLength))(gs)
      case _: EquipAction => CurrentEnemiesLens.modify(_.map(reduceEnemyStatusLength))(gs)
      case _ => gs
    }

  def reduceStatusLength(status: Seq[Status]): Seq[Status] =
    status.filter(_.turns > 0).map(s => s.copy(turns = s.turns - 1))

  def reduceEnemyStatusLength(e: Enemy): Enemy = {
    val activeStatus = e.status.filter(_.turns > 0)
    val altStatus = activeStatus.filter(ef => ef.effect == Burn)
    val toReduce = activeStatus.filterNot(ef => ef.effect == Burn)

    e.copy(status = reduceStatusLength(toReduce) ++ altStatus)
  }
}
