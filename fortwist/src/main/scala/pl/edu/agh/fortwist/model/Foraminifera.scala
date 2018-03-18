package pl.edu.agh.fortwist.model

import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class Foraminifera(energy: Energy, lifespan: Long)

object Foraminifera {
  def create()(implicit config: FortwistConfig): Foraminifera = Foraminifera(config.foraminiferaStartEnergy, 0)
}

final case class FortwistCell(smell: SmellArray, foraminiferas: Vector[Foraminifera], algae: Energy) extends SmellingCell {
  override type Self = FortwistCell

  override def withSmell(smell: SmellArray): FortwistCell = copy(smell = smell)
}

object FortwistCell {
  def create(foraminiferas: Vector[Foraminifera] = Vector.empty): FortwistCell =
    FortwistCell(Cell.emptySignal, foraminiferas, Energy.Zero)
}