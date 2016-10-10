package chousen

abstract class BaseCharacter extends Nameable with Stats with Attack {
  val isPlayer: Boolean

  def takeDamage(damage: Int): BaseCharacter


  def deathMessage: String = {
    s"$name dies"
  }
}

case class PlayerCharacter(name: String, maxHp: Int = 100, currentHp: Int = 100,
                           override val strength: Int = 8,
                           override val dexterity: Int = 8,
                           override val intellect: Int = 8,
                           override val vitality: Int = 8,
                           override val speed: Int = 8)
  extends BaseCharacter {

  override val isPlayer: Boolean = true

  override def takeDamage(damage: Int): BaseCharacter = {
    val currHp = this.currentHp - damage
    if (currHp <= 0) story(s"$name takes $damage")
    else statement(s"$name takes $damage and has ${currHp}HP left!")
    this.copy(currentHp = currHp)
  }
}

case class EnemyCharacter(name: String, maxHp: Int, currentHp: Int,
                          override val strength: Int = 8,
                          override val dexterity: Int = 8,
                          override val intellect: Int = 8,
                          override val vitality: Int = 8,
                          override val speed: Int = 8)
  extends BaseCharacter {

  override val isPlayer: Boolean = false

  def this(name: String, hp: Int) = {
    this(name, hp, hp)
  }

  override def takeDamage(damage: Int): BaseCharacter = {
    val currHp = this.currentHp - damage
    this.copy(currentHp = currHp)
  }
}

object EnemyCharacter {
  def create(name: String, maxHp: Int) = new EnemyCharacter(name, maxHp)

  def slime() = EnemyCharacter("Slime", 10, 10, strength = 4)

  def yellowSlime() = EnemyCharacter("Yellow Slime", 15, 15, vitality = 2)

  def giantSlime() = EnemyCharacter("Giant Slime", 30, 30, strength = 18, vitality = 12)
}


