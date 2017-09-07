package chousen.game.core

import chousen.api.data._
import chousen.game.actions.DamageCalculator
import chousen.game.status.{StatusBuilder, StatusCalculator}
import chousen.Optics._
import chousen.game.core.turn.PostTurnOps
import chousen.game.dungeon.EnemyBuilder

import scala.annotation.tailrec
import scala.util.{Left, Random, Right}


object GameOps extends GameOps(new EncounterOp(new StatusCalculator), new DamageCalculator(new StatusCalculator))

abstract class GameOps(encOps: EncounterOps, damageCalculator: DamageCalculator) {

  final def updateUntilPlayerIsActive(ed: EncounterData): EncounterData =
    updateUntilPlayerIsActive(ed._1, ed._2, ed._3)


  @scala.annotation.tailrec
  final def updateUntilPlayerIsActive(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage]): EncounterData = {
    val next: (Player, Set[Enemy], Seq[GameMessage]) = update(player, enemies, messages)
    encOps.getActive(next) match {
      case Left(_) => next
      case Right(_) =>
        val (p, es, msgs) = EnemyTurnOps.takeTurn(next._1, next._2, next._3)(damageCalculator)

        updateUntilPlayerIsActive(p, es, msgs)
    }
  }

  // Note Left side is inactive, Right is active
  def isGameActive(ed: EncounterData): Boolean = {
    val (player, enemies, _) = ed
    if (0 >= player.stats.currentHp || enemies.isEmpty) false
    else true
  }


  def update(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage]): EncounterData = {

    def process: EncounterUpdate = encOps.ensureActive _ andThen encOps.announceActive

    process(Tuple3(player, enemies, messages))
  }
}


object EnemyTurnOps {

  // This method will use the enemy with the highest position
  def takeTurn(player: Player, enemies: Set[Enemy], messages: Seq[GameMessage])(dc: DamageCalculator): (Player, Set[Enemy], Seq[GameMessage]) = {
    import chousen.Optics.EnemyStatusLens

    val activeEnemy = enemies.maxBy(_.position)
    val dmg = dc.calculateEnemyDamage(activeEnemy, player)


    val playerHp = PlayerOptics.PlayerCharStatsLens.composeLens(CharStatsOptics.HpLens)
    val hpLens: (Player) => Player = playerHp.modify(hp => hp - dmg)



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

    //TODO: Move elsewhere

    def diceRoll = Random.nextInt(6) + 1

    def doDamage(p: Player, es: Set[Enemy], messages: Seq[GameMessage], activeEnemy: Enemy) = {
      val attackMessage = if (dmg < 5) GameMessage(s"${activeEnemy.name} grazes ${player.name} for $dmg damage.")
      else if (dmg < 10) GameMessage(s"${activeEnemy.name} hits ${player.name} for $dmg damage.")
      else if (dmg < 20) GameMessage(s"${activeEnemy.name} attacks ${player.name} for $dmg damage!")
      else if (dmg < 30) GameMessage(s"${activeEnemy.name} smashes ${player.name} for $dmg damage!!")
      else if (dmg < 40) GameMessage(s"${activeEnemy.name} crushes ${player.name} for $dmg damage!!!")
      else if (dmg < 50) GameMessage(s"${activeEnemy.name} bops ${player.name} for $dmg damage!!!")
      else GameMessage(s"${activeEnemy.name} crits ${player.name} for $dmg damage!!!!!")

      // Then reset
      val es = finish(enemies, activeEnemy)
      val isTripleOrc = es.map(_.name).contains("TripleOrc") & es.size < 5
      val finalEs = if (isTripleOrc) es + EnemyBuilder.smallOrc else es

      val msg2 = if (isTripleOrc) Option(GameMessage(s"A Tiny Orc comes to the aid of TripleOrc")) else None


      (hpLens.apply(player), finalEs, (messages :+ attackMessage) ++ msg2)
    }


    val afterDmgGame = if (activeEnemy.name == "Warrior" && diceRoll == 6) {
      val message = GameMessage(s"${activeEnemy.name} blocks.")
      val es = finish(enemies, activeEnemy, Some(StatusBuilder.makeBlock()))
      (player, es, messages :+ message)
    } else if ((activeEnemy.name.contains("Orc") && activeEnemy.stats.currentHp <= 65) && diceRoll == 1) {
      val message = GameMessage(s"${activeEnemy.name} bursts into an Orcish Rage!")
      val es = finish(enemies, activeEnemy, Some(StatusBuilder.makeBerserk(2, turns = 2)))
      (player, es, messages :+ message)
    } else if (activeEnemy.name.contains("Steam Golem") && activeEnemy.stats.speed < 16 && diceRoll >= 5) {
      val message = GameMessage(s"Steam spouts from the ${activeEnemy.name} as it speeds up!")
      val es = finish(enemies, activeEnemy)
      val ez = es.map(e => if(e.id == activeEnemy.id) EnemyStatsLens.composeLens(SpeedLens).modify(i => i + 2).andThen(EnemyPosition.modify(_ - 100))(e) else e)
      (player, ez, messages :+ message)
    } else if (activeEnemy.name.contains("Orc Fighter") && !activeEnemy.status.map(_.effect).contains(Might) && diceRoll == 3) {
      val message = GameMessage(s"The Orc Fighter drinks a Potion of Might!")

      val es = finish(enemies, activeEnemy)
      val ez = es.map(e => if(e.id == activeEnemy.id) EnemyStatusLens.modify(i => i :+ StatusBuilder.makeMight(4))(activeEnemy) else e)

      (player, ez, messages :+ message)
    } else if (activeEnemy.name.contains("Orkish") && diceRoll == 4) {
      val message = GameMessage(s"${activeEnemy.name} casts Blockade!")
      val eMsgs =enemies.map(e => GameMessage(s"${e.name} is protected by a barrier"))

      val es = enemies.map(e => e.copy(status = e.status :+ StatusBuilder.makeBlock(turns = 1)))
        .map(e => if (e.id == activeEnemy.id) e.copy(position = e.position - 100) else e)

      (player, es, (messages :+ message) ++ eMsgs)
    } else if (activeEnemy.name.contains("Orkish") && diceRoll == 5) {
      val message = GameMessage(s"${activeEnemy.name} casts Mass Haste!")
      val eMsgs =enemies.map(e => GameMessage(s"${e.name} speeds up!"))

      val es = enemies.map(e => e.copy(status = e.status :+ StatusBuilder.makeHaste(4, turns = 3)))
        .map(e => if (e.id == activeEnemy.id) e.copy(position = e.position - 100) else e)

      (player, es, (messages :+ message) ++ eMsgs)
    } else if (activeEnemy.name.contains("Orkish") && diceRoll == 6) {
      val message = GameMessage(s"${activeEnemy.name} casts Mass Might!")
      val eMsgs =enemies.map(e => GameMessage(s"${e.name} becomes stronger!"))

      val es = enemies.map(e => e.copy(status = e.status :+ StatusBuilder.makeMight(4, turns = 3)))
          .map(e => if (e.id == activeEnemy.id) e.copy(position = e.position - 100) else e)
      (player, es, (messages :+ message) ++ eMsgs)
    }else if (activeEnemy.name.contains("Shaman") && diceRoll <= 2 && !player.status.map(_.effect).contains(Slow)) {
      val message = GameMessage(s"${activeEnemy.name} casts Quagmire!")
      val eMsgs = GameMessage(s"${player.name} is stuck in the Quagmire!")


      (PlayerStatusLens.modify(_ :+ StatusBuilder.makeSlow(2, turns = 3))(player), enemies, (messages :+ message) :+ eMsgs)
    } else if (activeEnemy.name.contains("Totem")) {
      val message = GameMessage(s"${activeEnemy.name} casts Barrier!")

      val ez = enemies.filterNot(_.id == activeEnemy.id)
        .map(e => EnemyStatusLens.modify(_ :+ StatusBuilder.makeBlock(turns = 1))(e))
      val eMsgs = ez.map(e => GameMessage(s"${e.name} is protected by a Barrier"))

      (player,
        ez + activeEnemy.copy(position = activeEnemy.position - 100),
        (messages :+ message) ++ eMsgs)
    } else if (activeEnemy.name.contains("Ancient")) {
      val message = GameMessage(s"${activeEnemy.name} grows stronger!")

      val newActive = EnemyStatsLens.composeLens(StrengthLens).modify(_ + 2).andThen(
        EnemyStatsLens.composeLens(DexterityLens).modify(_ + 2)
      )(activeEnemy)

      doDamage(player, enemies.map(e => if (e.id == newActive.id) newActive else e), messages :+ message, newActive)
    } else if (activeEnemy.name.contains("Kraken")) {
      val message = GameMessage(s"${activeEnemy.name} whips ${player.name} for $dmg!")

      (PlayerHealthLens.modify(_ - dmg)(player),
        enemies.map(e=> if (e.id == activeEnemy.id) e.copy(position = e.position - 50) else e),
        messages :+ message)
    }else {
      doDamage(player, enemies, messages, activeEnemy)
    }

    val statusHandler: ((Player, Set[Enemy], Seq[GameMessage])) => (Player, Set[Enemy], Seq[GameMessage]) =
      handlePerTurnStatuses(activeEnemy)
    statusHandler.andThen(PostTurnOps.handleDead)(afterDmgGame)
  }


  def handlePerTurnStatuses(ae: Enemy)(pem: (Player, Set[Enemy], Seq[GameMessage])) = {
    val (p, es, ms) = pem
    var msgs = Seq.empty[GameMessage]

    import cats.instances.option.catsKernelStdMonoidForOption
    import cats.instances.int.catsKernelStdGroupForInt
    import cats.implicits.catsSyntaxSemigroup

    def regenEffects(e: Enemy) = e.status.filter(s => s.effect == Regen)
      .reduceLeftOption[Status] { case (a, b) => a.copy(amount = a.amount |+| b.amount) }

    def burnEffects(e: Enemy) = e.status.filter(s => s.effect == Burn)
      .reduceLeftOption[Status] { case (a, b) => a.copy(amount = a.amount |+| b.amount) }

    def effectsForComputation(e: Enemy): Seq[Status] =
      e.status.filterNot(ef => ef.effect == Regen || ef.effect == Burn) ++ regenEffects(e) ++ burnEffects(e)

    def foldStatus(e: Enemy, s: Status) = {
      s.effect match {
        case Poison => e
        case Burn => {
          val dmg = s.amount.getOrElse(0) + p.experience.level
          def doDmg(i:Int) = Math.max(0, i - dmg)
          msgs = msgs :+ GameMessage(s"${e.name} burns for $dmg damage")

          EnemyHpLens.modify(doDmg)(e)
        }
        case Fear => {
          if (e.stats.currentHp < 25 || (e.stats.currentHp.toDouble / e.stats.maxHp.toDouble) <= 0.25) {
            EnemyHpLens.set(-666)(e)
          } else {
            e
          }
        }
        case _ => e
      }
    }
    def handleStatus(e: Enemy) =
      effectsForComputation(e).foldLeft(e)(foldStatus)


    def reducePerTurnStatus(e: Enemy) = e.copy(status = e.status.map(sf => sf.effect match {
      case Burn => sf.copy(turns = sf.turns - 1)
      case _ => sf
    }))

    def removeDeadStatuses(e: Enemy) = e.copy(status = e.status.filter(_.turns > 0))


    val updateEnemy: (Enemy) => Enemy = e => if(e.id == ae.id) {
      (handleStatus _).andThen(reducePerTurnStatus).andThen(removeDeadStatuses)(e) } else e

    (p, es.map(updateEnemy), ms ++ msgs)
   }
}


final class EncounterOp(sc: StatusCalculator) extends EncounterOps {

  @tailrec
  override def ensureActive(encounterData: EncounterData): EncounterData = {
    import chousen.Implicits._

    // At THIS point, status effects duration should be reduced -- since it would be PER tick.

    val (p, es, msgs) = encounterData
    val sePlayer = sc.calculate(p)

    val (player, enemies) = p.copy(position = p.position + sePlayer.stats.speed) ->
      es.map(e => e.copy(position = e.position + sc.calculate(e).stats.speed))

    val maxPosition = math.max(player.position, if (enemies.isEmpty) 0 else enemies.maxBy(_.position).position)
    lazy val numWithMaxPosition = if (player.position == maxPosition) 1 + enemies.count(_.position == maxPosition)
    else enemies.count(_.position == maxPosition)


    if (maxPosition < 100) ensureActive((player, enemies, msgs))
    else {
      lazy val withPosition = (es: Set[Enemy]) => es.filter(_.position >= maxPosition)
      lazy val enemiesWithPosition = withPosition(enemies)
      lazy val fastestEnemySpeed = enemiesWithPosition.map(e => sc.calculate(e)).maxBy(_.stats.speed).stats.speed

      numWithMaxPosition match {
        case 1 => (player, enemies, msgs)
        case 2 if player.position == maxPosition =>
          if (sePlayer.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val incEnemies: Set[Enemy] = enemies.map(e =>
              if (e ~= enemies.maxBy(_.position)) {
                e.copy(position = e.position + 1)
              } else e)

            ensureActive(Tuple3(player, incEnemies, msgs))
          }
        case _ if player.position == maxPosition =>
          if (sePlayer.stats.speed != fastestEnemySpeed) ensureActive((player, enemies, msgs))
          else {
            val fastestSpeeds = enemiesWithPosition.filter(_.stats.speed == fastestEnemySpeed)

            // Remove enemies without max speed (recurse)
            if (enemiesWithPosition.size > fastestSpeeds.size) {
              ensureActive(Tuple3(player, enemies, msgs))
            } else {
              // Player speed equals fastest enemy (a min of 2 have same speed)
              // All same speed
              val chosenOne: Enemy = enemiesWithPosition.maxBy(_.id)
              val nextEnemies: Set[Enemy] =
                enemies.map(e => if (e ~= chosenOne) e.copy(position = e.position + 1) else e)

              ensureActive(Tuple3(player, nextEnemies, msgs))
            }
          }
        case _ =>
          //enemiesWithPosition.map(e => e.copy(position = e.stats.speed + e.position))
          val fastestSpeeds = enemiesWithPosition.map(sc.calculate).filter(_.stats.speed == fastestEnemySpeed)

          fastestSpeeds.size match {
            case 1 => ensureActive((player, enemies, msgs))
            case _ =>
              if (fastestSpeeds.size < enemiesWithPosition.size) ensureActive((player, enemies, msgs))
              else {
                val chosenOne: Enemy = enemiesWithPosition.maxBy(_.id)
                val nextEnemies = enemies.map(e => if (e ~= chosenOne) e.copy(position = e.position + 1)
                else e)
                ensureActive((player, nextEnemies, msgs))
              }
          }
      }
    }
  }

  override def announceActive(encounterData: EncounterData): EncounterData = {
    val (player, enemies, msgs) = encounterData

    val fastestEnemy = if (enemies.isEmpty) 0 else enemies.maxBy(_.position).position

    val newMessages = if (player.position > fastestEnemy) msgs :+ GameMessage(s"${player.name}'s turn.")
    else msgs

    (player, enemies, newMessages)
  }

  override def getActive(encounterData: EncounterData): Either[Player, Enemy] = {
    val (p: Player, es: Set[Enemy], _) = encounterData
    val maxPosition = math.max(p.position, if (es.isEmpty) 0 else es.maxBy(_.position).position)

    if (p.position == maxPosition) Left(p)
    else Right(es.maxBy(_.position))
  }
}

trait EncounterOps {
  def ensureActive(encounterData: EncounterData): EncounterData

  def announceActive(encounterData: EncounterData): EncounterData

  def getActive(encounterData: EncounterData): Either[Player, Enemy]
}

