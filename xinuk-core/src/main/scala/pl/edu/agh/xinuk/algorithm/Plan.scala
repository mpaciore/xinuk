package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{CellContents, CellId, Direction}

final case class StateUpdate(updateTag: UpdateTag, value: CellContents)

object StateUpdate {
  def apply(value: CellContents): StateUpdate = new StateUpdate(UpdateTag.Default, value)
}

trait UpdateTag {
  def apply(contents: CellContents): StateUpdate = StateUpdate(this, contents)
}

object UpdateTag {

  case object Default extends UpdateTag

}

/*
 * action:      StateUpdate to be validated against current state of target cell and applied to it or rejected
 * consequence: StateUpdate to be applied to the source cell if action is applied
 * alternative: StateUpdate to be applied to the source cell if action is rejected
 */
final case class Plan(action: StateUpdate, consequence: Option[StateUpdate], alternative: Option[StateUpdate]) {
  def toTargeted(actionTarget: CellId, consequenceTarget: CellId, alternativeTarget: CellId): TargetedPlan =
    TargetedPlan(
      TargetedStateUpdate(actionTarget, action),
      consequence.map(TargetedStateUpdate(consequenceTarget, _)),
      alternative.map(TargetedStateUpdate(alternativeTarget, _))
    )
}

object Plan {
  def apply(action: StateUpdate, consequence: StateUpdate, alternative: StateUpdate): Plan =
    Plan(action, Some(consequence), Some(alternative))

  def apply(action: StateUpdate, consequence: StateUpdate): Plan =
    Plan(action, Some(consequence), None)

  def apply(action: StateUpdate): Plan = {
    Plan(action, None, None)
  }
}

final case class Plans(outwardsPlans: Map[Direction, Seq[Plan]], localPlans: Seq[Plan])

object Plans {
  def empty: Plans = Empty

  private def Empty: Plans = Plans(Map.empty, Seq.empty)

  def apply(outwardsPlans: Map[Direction, Seq[Plan]]): Plans = new Plans(outwardsPlans, Seq.empty)
}

final case class TargetedStateUpdate(target: CellId, update: StateUpdate)

final case class TargetedPlan(action: TargetedStateUpdate, consequence: Option[TargetedStateUpdate], alternative: Option[TargetedStateUpdate])
