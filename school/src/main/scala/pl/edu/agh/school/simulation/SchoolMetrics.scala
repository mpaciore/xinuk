package pl.edu.agh.school.simulation

import pl.edu.agh.xinuk.simulation.Metrics

/**
  * Metrics are used to measure actual statistics for eg. current students count
  */
final case class SchoolMetrics(studentsCount: Long,
                               teachersCount: Long,
                               cleanersCount: Long,
                               studentsDeaths: Long,
                               //                               foraminiferaTotalEnergy: Double,
                               //                               foraminiferaReproductionsCount: Long,
                               //                               consumedAlgaeCount: Long,
                               //                               foraminiferaTotalLifespan: Long,
                               //                               algaeTotalLifespan: Long
                              ) extends Metrics {

  override def log: String = {
    s"$studentsCount;$teachersCount;$cleanersCount$studentsDeaths"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Students" -> studentsCount,
    "Teachers" -> teachersCount,
    "Cleaners" -> cleanersCount
  )

  override def +(other: Metrics): SchoolMetrics = {
    other match {
      case SchoolMetrics.EMPTY => this
      case SchoolMetrics(otherStudentsCount, otherTeachersCount, otherCleanersCount, otherStudentDeaths,
//      otherForaminiferaTotalEnergy,
//      otherForaminiferaReproductionsCount, otherConsumedAlgaeCount, otherForaminiferaTotalLifespan,
//      otherAlgaeTotalLifespan
      ) =>
        SchoolMetrics(studentsCount + otherStudentsCount, teachersCount + otherTeachersCount,
          cleanersCount + otherCleanersCount, studentsDeaths + otherStudentDeaths,
//          foraminiferaTotalEnergy + otherForaminiferaTotalEnergy,
//          foraminiferaReproductionsCount + otherForaminiferaReproductionsCount,
//          consumedAlgaeCount + otherConsumedAlgaeCount, foraminiferaTotalLifespan + otherForaminiferaTotalLifespan,
//          algaeTotalLifespan + otherAlgaeTotalLifespan
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
