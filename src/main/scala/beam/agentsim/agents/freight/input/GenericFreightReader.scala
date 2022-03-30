package beam.agentsim.agents.freight.input

import beam.agentsim.agents.freight._
import beam.agentsim.agents.vehicles.{BeamVehicle, BeamVehicleType}
import beam.agentsim.infrastructure.taz.{TAZ, TAZTreeMap}
import beam.sim.common.GeoUtils
import beam.sim.config.BeamConfig.Beam.Agentsim.Agents.Freight
import beam.utils.SnapCoordinateUtils
import beam.utils.SnapCoordinateUtils._
import beam.utils.csv.GenericCsvReader
import beam.utils.matsim_conversion.MatsimPlanConversion.IdOps
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.lang3.StringUtils.isBlank
import org.matsim.api.core.v01.population._
import org.matsim.api.core.v01.{Coord, Id}
import org.matsim.households.Household

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * @author Dmitry Openkov
  */
class GenericFreightReader(
  val config: Freight,
  val geoUtils: GeoUtils,
  rnd: Random,
  tazTree: TAZTreeMap,
  snapLocationHelperMaybe: Option[SnapLocationHelper],
  outputDir: String
) extends LazyLogging
    with FreightReader {

  val freightIdPrefix = "freight"

  private def getRowValue(table: String, row: java.util.Map[String, String], key: String): String = {
    if (row.containsKey(key)) {
      row.get(key)
    } else {
      throw new IllegalArgumentException(s"Missing key '$key' in table '$table'.")
    }
  }

  @Override
  def readFreightTours(): Map[Id[FreightTour], FreightTour] = {
    val errors: ListBuffer[SnapLocationErrorInfo] = ListBuffer()
    val maybeTours = GenericCsvReader
      .readAsSeq[Option[FreightTour]](config.toursFilePath) { row =>
        def get(key: String): String = getRowValue(config.toursFilePath, row, key)
        // tourId,departureTimeInSec,departureLocationZone,departureLocationX,departureLocationY,maxTourDurationInSec
        val tourId: Id[FreightTour] = get("tourId").createId[FreightTour]
        val departureTimeInSec = get("departureTimeInSec").toInt
        val maxTourDurationInSec = get("maxTourDurationInSec").toInt
        val departureLocationX = row.get("departureLocationX")
        val departureLocationY = row.get("departureLocationY")

        // note: placeholder to update warehouseLocationTaz and warehouseLocationUTM later
        val freightTour = FreightTour(
          tourId,
          departureTimeInSec,
          None,
          new Coord(),
          maxTourDurationInSec
        )

        extractCoordOrTaz(
          tourId,
          departureLocationX,
          departureLocationY,
          row.get("departureLocationZone")
        ) match {
          case (departureLocationZoneMaybe, Left(departureLocationUTMOnMap)) =>
            Some(
              freightTour.copy(
                warehouseLocationTaz = departureLocationZoneMaybe,
                warehouseLocationUTM = departureLocationUTMOnMap
              )
            )
          case (departureLocationZoneMaybe, Right(Succeed)) =>
            val departureLocationUTMOnMap =
              getSnappedLocation(location(departureLocationX.toDouble, departureLocationY.toDouble))
            Some(
              freightTour.copy(
                warehouseLocationTaz = departureLocationZoneMaybe,
                warehouseLocationUTM = departureLocationUTMOnMap
              )
            )
          case (_, Right(OutOfBoundingBoxError(_, planCoord))) =>
            errors.append(
              SnapLocationErrorInfo(tourId.toString, ErrorOutOfBoundingBox, planCoord.getX, planCoord.getY)
            )
            logger.warn(
              "Following freight tour row discarded because departure location is out of bounding box: {}",
              row
            )
            None
          case (_, Right(R5SplitNullError(_, planCoord))) =>
            errors.append(SnapLocationErrorInfo(tourId.toString, ErrorR5SplitNull, planCoord.getX, planCoord.getY))
            logger.warn("Following freight tour row discarded because departure location is not reachable: {}", row)
            None
          case _ =>
            logger.error("Following freight tour row is discarded: {}", row)
            None
        }
      }

    SnapCoordinateUtils.writeToCsv(
      errors.toList,
      path = s"$outputDir/freightTourSnapLocationError.csv",
      idColumn = "tourId"
    )

    maybeTours.flatten
      .groupBy(_.tourId)
      .mapValues(_.head)
  }

  @Override
  def readPayloadPlans(): Map[Id[PayloadPlan], PayloadPlan] = {
    val errors: ListBuffer[SnapLocationErrorInfo] = ListBuffer()
    val maybePlans = GenericCsvReader
      .readAsSeq[Option[PayloadPlan]](config.plansFilePath) { row =>
        def get(key: String): String = getRowValue(config.plansFilePath, row, key)
        // payloadId,sequenceRank,tourId,payloadType,weightInKg,requestType,locationZone,locationX,locationY,
        // estimatedTimeOfArrivalInSec,arrivalTimeWindowInSecLower,arrivalTimeWindowInSecUpper,operationDurationInSec
        val requestType = get("requestType").toLowerCase() match {
          case "1" | "unloading" => FreightRequestType.Unloading
          case "0" | "loading"   => FreightRequestType.Loading
          case wrongValue =>
            throw new IllegalArgumentException(
              s"Value of requestType $wrongValue is unexpected."
            )
        }
        val operationDurationInSec = get("operationDurationInSec").toDouble.round.toInt
        val activityType = if (config.generateFixedActivitiesDurations) {
          s"${requestType.toString}|$operationDurationInSec"
        } else {
          requestType.toString
        }
        val payloadId = get("payloadId").createId[PayloadPlan]
        val locationX = row.get("locationX")
        val locationY = row.get("locationY")

        // note: placeholder to update locationZone and locationUTM later
        val payloadPlan = PayloadPlan(
          payloadId,
          get("sequenceRank").toDouble.round.toInt,
          get("tourId").createId,
          get("payloadType").createId[PayloadType],
          get("weightInKg").toDouble,
          requestType,
          activityType,
          None,
          new Coord(),
          get("estimatedTimeOfArrivalInSec").toDouble.toInt,
          get("arrivalTimeWindowInSecLower").toDouble.toInt,
          get("arrivalTimeWindowInSecUpper").toDouble.toInt,
          operationDurationInSec
        )

        extractCoordOrTaz(payloadId, locationX, locationY, row.get("locationZone")) match {
          case (locationZoneMaybe, Left(locationUTMOnMap)) =>
            Some(payloadPlan.copy(locationZone = locationZoneMaybe, locationUTM = locationUTMOnMap))
          case (locationZoneMaybe, Right(Succeed)) =>
            val locationUTMOnMap = getSnappedLocation(location(locationX.toDouble, locationY.toDouble))
            Some(payloadPlan.copy(locationZone = locationZoneMaybe, locationUTM = locationUTMOnMap))
          case (_, Right(OutOfBoundingBoxError(_, planCoord))) =>
            errors.append(
              SnapLocationErrorInfo(payloadId.toString, ErrorOutOfBoundingBox, planCoord.getX, planCoord.getY)
            )
            logger.warn("Following freight plan row discarded because location is out of bounding box: {}", row)
            None
          case (_, Right(R5SplitNullError(_, planCoord))) =>
            errors.append(SnapLocationErrorInfo(payloadId.toString, ErrorR5SplitNull, planCoord.getX, planCoord.getY))
            logger.warn("Following freight plan row discarded because location is not reachable: {}", row)
            None
          case _ =>
            logger.error("Following freight plan row is discarded: {}", row)
            None
        }
      }

    SnapCoordinateUtils.writeToCsv(
      errors.toList,
      path = s"$outputDir/freightPayloadSnapLocationError.csv",
      idColumn = "payloadId"
    )

    maybePlans.flatten
      .groupBy(_.payloadId)
      .mapValues(_.head)
  }

  @Override
  def readFreightCarriers(
    allTours: Map[Id[FreightTour], FreightTour],
    allPlans: Map[Id[PayloadPlan], PayloadPlan],
    vehicleTypes: Map[Id[BeamVehicleType], BeamVehicleType]
  ): IndexedSeq[FreightCarrier] = {
    val errors: ListBuffer[SnapLocationErrorInfo] = ListBuffer()
    val existingTours: Set[Id[FreightTour]] = allTours.keySet.intersect(allPlans.map(_._2.tourId).toSet)
    val plans: Map[Id[PayloadPlan], PayloadPlan] = allPlans.filter { case (_, plan) =>
      existingTours.contains(plan.tourId)
    }
    val tours: Map[Id[FreightTour], FreightTour] = allTours.filter { case (_, tour) =>
      existingTours.contains(tour.tourId)
    }

    case class FreightCarrierRow(
      carrierId: Id[FreightCarrier],
      tourId: Id[FreightTour],
      vehicleId: Id[BeamVehicle],
      vehicleTypeId: Id[BeamVehicleType],
      warehouseLocationZone: Option[Id[TAZ]],
      warehouseLocationUTM: Coord
    )

    def createCarrierVehicles(
      carrierId: Id[FreightCarrier],
      carrierRows: IndexedSeq[FreightCarrierRow],
      warehouseLocationUTM: Coord
    ): IndexedSeq[BeamVehicle] = {
      val vehicles: IndexedSeq[BeamVehicle] = carrierRows
        .groupBy(_.vehicleId)
        .map { case (vehicleId, rows) =>
          val firstRow = rows.head
          val vehicleType = vehicleTypes.getOrElse(
            firstRow.vehicleTypeId,
            throw new IllegalArgumentException(
              s"Vehicle type for vehicle $vehicleId not found: ${firstRow.vehicleTypeId}"
            )
          )
          if (vehicleType.payloadCapacityInKg.isEmpty)
            throw new IllegalArgumentException(
              s"Vehicle type ${firstRow.vehicleTypeId} for vehicle $vehicleId has no payloadCapacityInKg defined"
            )
          createFreightVehicle(vehicleId, vehicleType, carrierId, warehouseLocationUTM, rnd.nextInt())
        }
        .toIndexedSeq
      vehicles
    }

    def createCarrier(carrierId: Id[FreightCarrier], carrierRows: IndexedSeq[FreightCarrierRow]) = {
      val warehouseLocationUTM: Coord = carrierRows.head.warehouseLocationUTM
      val vehicles: scala.IndexedSeq[BeamVehicle] = createCarrierVehicles(carrierId, carrierRows, warehouseLocationUTM)
      val vehicleMap: Map[Id[BeamVehicle], BeamVehicle] = vehicles.map(vehicle => vehicle.id -> vehicle).toMap

      val tourMap: Map[Id[BeamVehicle], IndexedSeq[FreightTour]] = carrierRows
        .groupBy(_.vehicleId)
        .mapValues { rows =>
          rows
            //setting the tour warehouse location to be the carrier warehouse location
            .map(row => tours(row.tourId).copy(warehouseLocationUTM = warehouseLocationUTM))
            .sortBy(_.departureTimeInSec)
        }

      val carrierTourIds = tourMap.values.flatten.map(_.tourId).toSet

      val plansPerTour: Map[Id[FreightTour], IndexedSeq[PayloadPlan]] =
        plans.values.groupBy(_.tourId).filterKeys(carrierTourIds).mapValues(_.toIndexedSeq.sortBy(_.sequenceRank))
      val carrierPlanIds: Set[Id[PayloadPlan]] = plansPerTour.values.flatten.map(_.payloadId).toSet
      val payloadMap = plans.filterKeys(carrierPlanIds)

      FreightCarrier(carrierId, tourMap, payloadMap, vehicleMap, plansPerTour)
    }

    val maybeCarrierRows = GenericCsvReader.readAsSeq[Option[FreightCarrierRow]](config.carriersFilePath) { row =>
      def get(key: String): String = getRowValue(config.carriersFilePath, row, key)
      //carrierId,tourId,vehicleId,vehicleTypeId,warehouseZone,warehouseX,warehouseY
      val carrierId: Id[FreightCarrier] = s"$freightIdPrefix-carrier-${get("carrierId")}".createId
      val tourId: Id[FreightTour] = get("tourId").createId
      val vehicleId: Id[BeamVehicle] = Id.createVehicleId(s"$freightIdPrefix-vehicle-${get("vehicleId")}")
      val vehicleTypeId: Id[BeamVehicleType] = get("vehicleTypeId").createId
      if (!existingTours.contains(tourId)) {
        logger.error("Following freight carrier row discarded because tour {} was filtered out: {}", tourId, row)
        None
      } else {
        val warehouseX = row.get("warehouseX")
        val warehouseY = row.get("warehouseY")

        // note: placeholder to update warehouseLocationZone and warehouseLocationUTM later
        val freightCarrier = FreightCarrierRow(carrierId, tourId, vehicleId, vehicleTypeId, None, new Coord())

        extractCoordOrTaz(carrierId, warehouseX, warehouseY, row.get("warehouseZone")) match {
          case (warehouseZoneMaybe, Left(warehouseUTMOnMap)) =>
            Some(
              freightCarrier.copy(warehouseLocationZone = warehouseZoneMaybe, warehouseLocationUTM = warehouseUTMOnMap)
            )
          case (warehouseZoneMaybe, Right(Succeed)) =>
            val warehouseUTMOnMap = getSnappedLocation(location(warehouseX.toDouble, warehouseY.toDouble))
            Some(
              freightCarrier.copy(warehouseLocationZone = warehouseZoneMaybe, warehouseLocationUTM = warehouseUTMOnMap)
            )
          case (_, Right(OutOfBoundingBoxError(_, planCoord))) =>
            errors.append(
              SnapLocationErrorInfo(carrierId.toString, ErrorOutOfBoundingBox, planCoord.getX, planCoord.getY)
            )
            logger.warn(
              "Following freight carrier row discarded because warehouse location is out of bounding box: {}",
              row
            )
            None
          case (_, Right(R5SplitNullError(_, planCoord))) =>
            errors.append(SnapLocationErrorInfo(carrierId.toString, ErrorR5SplitNull, planCoord.getX, planCoord.getY))
            logger.warn("Following freight carrier row discarded because warehouse location is not reachable: {}", row)
            None
          case _ =>
            logger.error("Following freight carrier row is discarded: {}", row)
            None
        }
      }
    }

    val carriersWithFleet = maybeCarrierRows.flatten
      .groupBy(_.carrierId)
      .map { case (carrierId, carrierRows) =>
        createCarrier(carrierId, carrierRows)
      }
      .toIndexedSeq

    SnapCoordinateUtils.writeToCsv(
      errors.toList,
      path = s"$outputDir/freightCarrierSnapLocationError.csv",
      idColumn = "carrierId"
    )

    carriersWithFleet
  }

  private def getSnappedLocation(coord: Coord): Coord =
    snapLocationHelperMaybe.get
      .store(coord)
      .get

  private def getTaz(tazId: String): TAZ = tazTree.getTAZ(tazId) match {
    case Some(taz) => taz
    case None      => throw new IllegalArgumentException(s"Cannot find taz with id $tazId")
  }

  private def getDistributedTazLocation(taz: TAZ): Coord =
    convertedLocation(TAZTreeMap.randomLocationInTAZ(taz, rnd))

  private def computeLocationResult[A](id: Id[A], wgsCoord: Coord): Result = {
    snapLocationHelperMaybe.map(_.computeResult(id, wgsCoord)).getOrElse(Succeed)
  }

  // either[tazLocation, snapLocationResult]
  type ClosestUTMPoint = Either[Coord, Result]

  private def extractCoordOrTaz[A](
    id: Id[A],
    strX: String,
    strY: String,
    strZone: String
  ): (Option[Id[TAZ]], ClosestUTMPoint) = {
    if (isBlank(strX) || isBlank(strY)) {
      val taz = getTaz(strZone)
      (Some(taz.tazId), Left(getDistributedTazLocation(taz)))
    } else {
      (None, Right(computeLocationResult(id, location(strX.toDouble, strY.toDouble))))
    }
  }

  @Override
  def createPersonId(vehicleId: Id[BeamVehicle]): Id[Person] = {
    if (vehicleId.toString.startsWith(freightIdPrefix)) {
      Id.createPersonId(s"$vehicleId-agent")
    } else {
      Id.createPersonId(s"$freightIdPrefix-$vehicleId-agent")
    }
  }

  @Override
  def createHouseholdId(vehicleId: Id[BeamVehicle]): Id[Household] = {
    if (vehicleId.toString.startsWith(freightIdPrefix)) {
      s"$vehicleId-household".createId
    } else {
      s"$freightIdPrefix-$vehicleId-household".createId
    }
  }

}
