package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.fortwist.algorithm.FortwistUpdateTag.{Add, Change, Remove}
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.{Foraminifera, Seabed}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.CellContents

final case class FortwistPlanResolver() extends PlanResolver[FortwistConfig] {
  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: FortwistConfig): Boolean =
    (contents, update.updateTag, update.value) match {
      case (_: Seabed, Add, _: Seabed) => true
      case (_: Seabed, Remove, _: Seabed) => true
      case (_: Seabed, Change, _: Seabed) => true
      case _ => false
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: FortwistConfig): (CellContents, FortwistMetrics) = {
    val newContents: CellContents = (contents, update.updateTag, update.value) match {
      case (Seabed(foraminiferas, algae), Add, Seabed(addedForaminiferas, algaeDiff)) =>
        val newForaminiferas = foraminiferas ++ addedForaminiferas
        Seabed(newForaminiferas, algae + algaeDiff)

      case (Seabed(foraminiferas, algae), Remove, Seabed(removedForaminiferas, algaeDiff)) =>
        val newForaminiferas = foraminiferas.filterNot {
          foraminifera => removedForaminiferas.exists(_.id == foraminifera.id)
        }
        Seabed(newForaminiferas, algae + algaeDiff)

      case (Seabed(foraminiferas, algae), Change, Seabed(changedForaminiferas, algaeDiff)) =>
        val newForaminiferas: Seq[Foraminifera] = foraminiferas.map {
          foraminifera => changedForaminiferas.find(_.id == foraminifera.id).getOrElse(foraminifera)
        }
        Seabed(newForaminiferas, algae + algaeDiff)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }

    (newContents, FortwistMetrics.empty)
  }
}
