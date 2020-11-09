package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{CellId, Direction}

trait Update

/*
 * action:      StateUpdate to be validated against current state of target cell and applied to it or rejected
 * consequence: StateUpdate to be applied to the source cell if action is applied
 * alternative: StateUpdate to be applied to the source cell if action is rejected
 */
final case class Plan(action: Update, consequence: Option[Update], alternative: Option[Update]) {
  def toTargeted(actionTarget: CellId, consequenceTarget: CellId, alternativeTarget: CellId): TargetedPlan =
    TargetedPlan(
      TargetedStateUpdate(actionTarget, action),
      consequence.map(TargetedStateUpdate(consequenceTarget, _)),
      alternative.map(TargetedStateUpdate(alternativeTarget, _))
    )
}

object Plan {
  def apply(action: Update, consequence: Update, alternative: Update): Plan =
    Plan(action, Some(consequence), Some(alternative))

  def apply(action: Update, consequence: Update): Plan =
    Plan(action, Some(consequence), None)

  def apply(action: Update): Plan = {
    Plan(action, None, None)
  }
}

final case class Plans(plans: Seq[(Option[Direction], Plan)] = Seq.empty) {
  def ++(other: Plans): Plans = {
    Plans(plans ++ other.plans)
  }

  def :+(plan: (Option[Direction], Plan)): Plans = {
    Plans(plans :+ plan)
  }

  def outwardsPlans: Map[Direction, Seq[Plan]] = {
    plans.filter(_._1.isDefined)
      .map { case (dirOpt, plan) => (dirOpt.get, plan) }
      .groupBy(_._1)
      .map {case (dir, groups) => (dir, groups.map(_._2)) }
  }

  def localPlans: Seq[Plan] = {
    plans.filter(_._1.isEmpty)
      .map(_._2)
  }
}

object Plans {
  private val Empty: Plans = Plans()

  def empty: Plans = Empty

  def apply(plan: (Option[Direction], Plan)): Plans = new Plans(Seq(plan))
}

final case class TargetedStateUpdate(target: CellId, update: Update)

final case class TargetedPlan(action: TargetedStateUpdate, consequence: Option[TargetedStateUpdate], alternative: Option[TargetedStateUpdate])
