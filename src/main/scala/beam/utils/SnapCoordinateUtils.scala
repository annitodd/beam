package beam.utils

import beam.sim.common.GeoUtils
import beam.utils.csv.CsvWriter
import com.conveyal.r5.streets.StreetLayer
import com.typesafe.scalalogging.LazyLogging
import org.matsim.api.core.v01.{Coord, Id}

import scala.collection.concurrent.TrieMap

object SnapCoordinateUtils extends LazyLogging {

  trait Result
  final case class OutOfBoundingBoxError[A](id: Id[A], coord: Coord) extends Result
  final case class R5SplitNullError[A](id: Id[A], coord: Coord) extends Result
  final case object Succeed extends Result

  case class SnapLocationHelper(geo: GeoUtils, streetLayer: StreetLayer, maxRadius: Double) {
    val store: TrieMap[Coord, Option[Coord]] = TrieMap.empty

    def computeResult[A](id: Id[A], utmCoord: Coord): Result = {
      val wgsCoord = geo.utm2Wgs(utmCoord)
      if (streetLayer.envelope.contains(wgsCoord.getX, wgsCoord.getY)) {
        val snapCoordOpt = store.getOrElseUpdate(
          utmCoord,
          Option(geo.getR5Split(streetLayer, wgsCoord, maxRadius)).map { split =>
            val updatedPlanCoord = geo.splitToCoord(split)
            geo.wgs2Utm(updatedPlanCoord)
          }
        )
        snapCoordOpt.fold[Result](R5SplitNullError(id, utmCoord))(_ => Succeed)
      } else OutOfBoundingBoxError(id, utmCoord)
    }
  }

  val ErrorOutOfBoundingBox = "OutOfBoundingBox"
  val ErrorR5SplitNull = "R5SplitNull"

  // error csv row
  case class SnapLocationErrorInfo(id: String, error: String, planX: Double, planY: Double)

  def writeToCsv(errors: List[SnapLocationErrorInfo], path: String, idColumn: String): Unit = {
    new CsvWriter(path, idColumn, "error", "x", "y")
      .writeAllAndClose(
        errors.map(error => List(error.id, error.error, error.planX, error.planY))
      )
    logger.info("See location error info at {}.", path)
  }

}
