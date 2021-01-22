package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{CellId, Direction}

trait Update

/*
 * action:      Update to be validated against current state of target cell and applied to it or rejected
 * consequence: optional Update to be applied to the source cell if action is applied
 * alternative: optional Update to be applied to the source cell if action is rejected
 */
final case class Plan(action: Update, consequence: Option[Update], alternative: Option[Update]) {
  def toTargeted(planSource: CellId, actionTarget: CellId, consequenceTarget: CellId, alternativeTarget: CellId): TargetedPlan =
    TargetedPlan(
      TargetedUpdate(actionTarget, planSource, action),
      consequence.map(TargetedUpdate(consequenceTarget, planSource, _)),
      alternative.map(TargetedUpdate(alternativeTarget, planSource, _))
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

final case class TargetedUpdate(target: CellId, source: CellId, update: Update)

final case class TargetedPlan(action: TargetedUpdate, consequence: Option[TargetedUpdate], alternative: Option[TargetedUpdate])
