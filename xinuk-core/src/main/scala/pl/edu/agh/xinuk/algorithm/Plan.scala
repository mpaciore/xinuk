package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{CellId, CellState}

final case class StateUpdate(value: CellState) extends AnyVal

final case class Plan(action: StateUpdate, consequence: Option[StateUpdate]) {
  def toTargeted(actionTarget: CellId, consequenceTarget: CellId): TargetedPlan =
    TargetedPlan(TargetedStateUpdate(actionTarget, action), consequence.map(TargetedStateUpdate(consequenceTarget, _)))
}

object Plan {
  def apply(action: StateUpdate, consequence: StateUpdate): Plan = Plan(action, Some(consequence))
  def apply(action: StateUpdate): Plan = Plan(action, None)
}

final case class TargetedStateUpdate(target: CellId, update: StateUpdate)

final case class TargetedPlan(action: TargetedStateUpdate, consequence: Option[TargetedStateUpdate])
