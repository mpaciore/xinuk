package pl.edu.agh.school.simulation

import pl.edu.agh.xinuk.simulation.Metrics

/**
  * Metrics are used to measure actual statistics for eg. current students count
  */
final case class SchoolMetrics(studentsCount: Long,
                               teachersCount: Long,
                               cleanersCount: Long,
                               dirtCount: Long
                              ) extends Metrics {

  override def log: String = {
    s"$studentsCount;$teachersCount;$cleanersCount;$dirtCount"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Students" -> studentsCount,
    "Teachers" -> teachersCount,
    "Cleaners" -> cleanersCount,
    "Dirt"     -> dirtCount
  )

  override def +(other: Metrics): SchoolMetrics = {
    other match {
      case SchoolMetrics.EMPTY => this
      case SchoolMetrics(otherStudentsCount, otherTeachersCount, otherCleanersCount, otherDirtCount
      ) =>
        SchoolMetrics(studentsCount + otherStudentsCount, teachersCount + otherTeachersCount,
          cleanersCount + otherCleanersCount, dirtCount + otherDirtCount
        )
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-SchoolMetrics to SchoolMetrics")
    }
  }
}

object SchoolMetrics {
  private val EMPTY = SchoolMetrics(0, 0, 0, 0)

  def empty(): SchoolMetrics = EMPTY
}
