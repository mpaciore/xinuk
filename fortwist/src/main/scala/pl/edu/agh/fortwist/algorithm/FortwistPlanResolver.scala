package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.fortwist.algorithm.FortwistUpdate.SeabedUpdate
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.Seabed
import pl.edu.agh.xinuk.algorithm.{PlanResolver, Update}
import pl.edu.agh.xinuk.model.CellContents

final case class FortwistPlanResolver() extends PlanResolver[FortwistConfig] {
  override def isUpdateValid(iteration: Long, contents: CellContents, update: Update)(implicit config: FortwistConfig): Boolean =
    (contents, update) match {
      case (_: Seabed, _: SeabedUpdate) => true
      case _ => false
    }

  override def applyUpdate(iteration: Long, contents: CellContents, update: Update)(implicit config: FortwistConfig): (CellContents, FortwistMetrics) = {
    val newContents: CellContents = (contents, update) match {
      case (Seabed(foraminiferas, algae), SeabedUpdate(foraminiferasToAdd, foraminiferasToRemove, foraminiferaReplacements, algaeDiff)) =>
        val remainingForaminiferas = foraminiferas.filterNot {
          foraminifera => foraminiferasToRemove.contains(foraminifera.id)
        }.map {
          foraminifera => foraminiferaReplacements.find(_.id == foraminifera.id).getOrElse(foraminifera)
        }
        Seabed(remainingForaminiferas ++ foraminiferasToAdd, algae + algaeDiff)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }

    (newContents, FortwistMetrics.empty)
  }
}
