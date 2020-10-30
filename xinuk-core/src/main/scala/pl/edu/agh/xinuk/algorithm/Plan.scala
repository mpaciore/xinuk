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

final case class Plans(outwardsPlans: Map[Direction, Seq[Plan]], localPlans: Seq[Plan])

object Plans {
  def empty: Plans = Empty

  private def Empty: Plans = Plans(Map.empty, Seq.empty)

  def apply(outwardsPlans: Map[Direction, Seq[Plan]]): Plans = new Plans(outwardsPlans, Seq.empty)
}

final case class TargetedStateUpdate(target: CellId, update: Update)

final case class TargetedPlan(action: TargetedStateUpdate, consequence: Option[TargetedStateUpdate], alternative: Option[TargetedStateUpdate])
