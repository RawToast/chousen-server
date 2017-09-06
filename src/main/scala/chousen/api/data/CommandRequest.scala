package chousen.api.data

import java.util.UUID

sealed trait CommandRequest

final case class AttackRequest(targetId: UUID) extends CommandRequest

final case class BlockRequest() extends CommandRequest

final case class SelfInflictingActionRequest(action: SelfAction) extends CommandRequest

final case class SingleTargetActionRequest(targetId: UUID, action: SingleTargetAction) extends CommandRequest

final case class MultiTargetActionRequest(targetIds: Set[UUID], action: MultiAction) extends CommandRequest

final case class CardActionRequest(action: CardAction, cardId: Option[UUID]) extends CommandRequest

final case class CampfireActionRequest(action: CampFireAction, cardId: Option[UUID]) extends CommandRequest

final case class EquipmentActionRequest(id: UUID, action: EquipAction) extends CommandRequest


sealed trait Action

sealed trait SingleTargetAction extends Action
sealed trait MultiAction extends Action
sealed trait SelfAction extends Action
sealed trait CardAction extends Action
sealed trait CampFireAction extends Action


sealed trait StandardCardAction extends CardAction
sealed trait DiscardCardAction extends CardAction

sealed trait EquipAction extends Action

sealed trait EquipWeapon extends EquipAction
sealed trait EquipArmour extends EquipAction
sealed trait EquipJewelery extends EquipAction


case object CrushingBlow extends SingleTargetAction
case object BurningHammer extends SingleTargetAction
case object StunningStrike extends SingleTargetAction
case object Counter extends SingleTargetAction
case object Destruction extends SingleTargetAction
case object QuickAttack extends SingleTargetAction
case object Assassinate extends SingleTargetAction
case object Pain extends SingleTargetAction
case object MagicMissile extends SingleTargetAction
case object Ember extends SingleTargetAction
case object LifeSwap extends SingleTargetAction


case object Fireball extends MultiAction
case object Extinguish extends MultiAction
case object PotionOfFlames extends MultiAction
case object ScrollOfFear extends MultiAction
case object MassDrain extends MultiAction
case object Shatter extends MultiAction
case object GroundStrike extends MultiAction
case object WindStrike extends MultiAction

case object Barrier extends SelfAction
case object HealWounds extends SelfAction
case object Haste extends SelfAction

case object PotionOfMight extends SelfAction
case object PotionOfDexterity extends SelfAction
case object PotionOfIntelligence extends SelfAction
case object PotionOfStoneSkin extends SelfAction
case object PotionOfRage extends SelfAction
case object PotionOfTrogg extends SelfAction
case object PotionOfContinuation extends SelfAction
case object PotionOfRegeneration extends SelfAction
case object PotionOfLignification extends SelfAction


case object ElixirOfStrength extends SelfAction
case object ElixirOfDexterity extends SelfAction
case object ElixirOfIntelligence extends SelfAction
case object ElixirOfVitality extends SelfAction
case object RarePepe extends SelfAction
case object QuickStep extends SelfAction

case object EssenceOfStrength extends SelfAction
case object EssenceOfDexterity extends SelfAction
case object EssenceOfIntelligence extends SelfAction
case object EssenceOfVitality extends SelfAction


case object Rummage extends StandardCardAction
case object Miracle extends StandardCardAction
case object Replace extends StandardCardAction
case object Restore extends StandardCardAction
case object Refresh extends StandardCardAction
case object Armoury extends StandardCardAction
case object Recharge extends StandardCardAction


case object ForgeArmour extends DiscardCardAction
case object ForgeWeapon extends DiscardCardAction
case object Trade extends DiscardCardAction
case object ManifestRage extends DiscardCardAction
case object EssenceBoost extends DiscardCardAction
case object ReduceRequirements extends DiscardCardAction
case object IncreaseCharges extends DiscardCardAction

sealed trait DiscardingCampFireAction extends CampFireAction
case object Rest extends CampFireAction
case object Explore extends CampFireAction
case object RestAndExplore extends CampFireAction
case object Drop extends DiscardingCampFireAction
case object Destroy extends DiscardingCampFireAction


case object Club extends EquipWeapon
case object Mace extends EquipWeapon
case object ShortSword extends EquipWeapon
case object BroadSword extends EquipWeapon
case object LongSword extends EquipWeapon
case object GiantClub extends EquipWeapon
case object TrollCrusher extends EquipWeapon
case object SwordOfIntellect extends EquipWeapon
case object DaggerOfDavid extends EquipWeapon


case object Cape extends EquipArmour
case object LeatherArmour extends EquipArmour
case object Ringmail extends EquipArmour
case object Chainmail extends EquipArmour
case object HeavyArmour extends EquipArmour
case object OrcishArmour extends EquipArmour
