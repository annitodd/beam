package beam.router.skim.core

import beam.agentsim.events.RideHailReservationConfirmationEvent.{Pooled, RideHailReservationType, Solo}
import beam.agentsim.infrastructure.taz.TAZ
import beam.router.skim.Skims
import beam.router.skim.core.AbstractSkimmer.Aggregator
import beam.router.skim.readonly.RideHailSkims
import beam.sim.config.BeamConfig
import beam.utils.matsim_conversion.MatsimPlanConversion.IdOps
import com.google.inject.Inject
import com.typesafe.scalalogging.LazyLogging
import org.matsim.api.core.v01.Id
import org.matsim.core.controler.MatsimServices

/**
  * @author Dmitry Openkov
  */
class RideHailSkimmer @Inject() (
  matsimServices: MatsimServices,
  beamConfig: BeamConfig
) extends AbstractSkimmer(beamConfig, matsimServices.getControlerIO) {
  import RideHailSkimmer._
  override protected[skim] val readOnlySkim = new RideHailSkims
  override protected val skimFileBaseName: String = RideHailSkimmer.fileBaseName

  override protected val skimFileHeader =
    "tazId,hour,reservationType,waitTime,costPerMile,unmatchedRequestsPercent,observations,iterations"
  override protected val skimName: String = RideHailSkimmer.name
  override protected val skimType: Skims.SkimType.Value = Skims.SkimType.TC_SKIMMER

  override protected def fromCsv(
    line: collection.Map[String, String]
  ): (AbstractSkimmerKey, AbstractSkimmerInternal) = {
    (
      RidehailSkimmerKey(
        tazId = line("tazId").createId,
        hour = line("hour").toInt,
        reservationType = if (line("reservationType").equalsIgnoreCase("pooled")) Pooled else Solo
      ),
      RidehailSkimmerInternal(
        waitTime = line("waitTime").toDouble,
        costPerMile = line("costPerMile").toDouble,
        unmatchedRequestsPercent = line("unmatchedRequestsPercent").toDouble,
        observations = line("observations").toInt,
        iterations = line("iterations").toInt
      )
    )
  }

  override protected def aggregateOverIterations(
    prevIteration: Option[AbstractSkimmerInternal],
    currIteration: Option[AbstractSkimmerInternal]
  ): AbstractSkimmerInternal =
    AbstractSkimmer.aggregateOverIterations[RidehailSkimmerInternal](prevIteration, currIteration) { agg =>
      RidehailSkimmerInternal(
        waitTime = agg.aggregate(_.waitTime),
        costPerMile = agg.aggregate(_.costPerMile),
        unmatchedRequestsPercent = agg.aggregate(_.unmatchedRequestsPercent),
        observations = agg.aggregate(_.observations),
        iterations = agg.aggregateObservations
      )
    }

  override protected def aggregateWithinIteration(
    prevObservation: Option[AbstractSkimmerInternal],
    currObservation: AbstractSkimmerInternal
  ): AbstractSkimmerInternal =
    AbstractSkimmer.aggregateWithinIteration[RidehailSkimmerInternal](prevObservation, currObservation) { agg =>
      RidehailSkimmerInternal(
        waitTime = agg.aggregate(_.waitTime),
        costPerMile = agg.aggregate(_.costPerMile),
        unmatchedRequestsPercent = agg.aggregate(_.unmatchedRequestsPercent),
        observations = agg.aggregateObservations
      )
    }
}

object RideHailSkimmer extends LazyLogging {
  val name = "ridehail-skimmer"
  val fileBaseName = "skimsRidehail"

  case class RidehailSkimmerKey(tazId: Id[TAZ], hour: Int, reservationType: RideHailReservationType)
      extends AbstractSkimmerKey {
    override def toCsv: String = productIterator.mkString(",")
  }

  case class RidehailSkimmerInternal(
    waitTime: Double,
    costPerMile: Double,
    unmatchedRequestsPercent: Double,
    observations: Int = 1,
    iterations: Int = 1
  ) extends AbstractSkimmerInternal {
    override def toCsv: String = AbstractSkimmer.toCsv(productIterator)
  }

}
