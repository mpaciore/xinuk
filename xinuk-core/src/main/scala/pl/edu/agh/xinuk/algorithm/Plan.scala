package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{CellId, CellState}

final case class StateUpdate(value: CellState) extends AnyVal

final case class Plan(action: StateUpdate, consequence: StateUpdate) {
  def toTargeted(actionTarget: CellId, consequenceTarget: CellId): TargetedPlan =
    TargetedPlan(TargetedStateUpdate(actionTarget, action), TargetedStateUpdate(consequenceTarget, consequence))
}

final case class TargetedStateUpdate(target: CellId, update: StateUpdate)

final case class TargetedPlan(action: TargetedStateUpdate, consequence: TargetedStateUpdate)
