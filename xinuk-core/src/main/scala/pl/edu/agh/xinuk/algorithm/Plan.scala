package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.Cell

final class MovePlan(val action: Move, val consequence: Move)

object MovePlan {
  def apply(action: Move, consequence: Move): MovePlan =
    new MovePlan(action, consequence)

  def unapply(arg: MovePlan): (Move, Move) = (arg.action, arg.consequence)
}

final class Move(val update: Cell, val targetCoordinates: (Int, Int))

object Move {
  def apply(update: Cell, targetCoordinates: (Int, Int)): Move =
    new Move(update, targetCoordinates)

  def unapply(arg: Move): (Cell, (Int, Int)) = (arg.update, arg.targetCoordinates)
}
