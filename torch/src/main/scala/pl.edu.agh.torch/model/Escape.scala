package pl.edu.agh.torch.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.SmellingCell

final case class EscapeCell(smell: SmellArray) extends SmellingCell {
  override type Self = EscapeCell

  override def withSmell(smell: SmellArray): EscapeCell = copy(smell = smell)
}
