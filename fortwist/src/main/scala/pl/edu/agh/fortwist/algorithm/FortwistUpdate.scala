package pl.edu.agh.fortwist.algorithm

import java.util.UUID

import pl.edu.agh.fortwist.model.Foraminifera
import pl.edu.agh.xinuk.algorithm.Update

object FortwistUpdate {

  case class SeabedUpdate(
                           foraminiferasToAdd: Seq[Foraminifera] = Seq.empty,
                           foraminiferasToRemove: Seq[UUID] = Seq.empty,
                           foraminiferasToChange: Seq[Foraminifera] = Seq.empty,
                           algaeDiff: Double = 0
                         ) extends Update

}
