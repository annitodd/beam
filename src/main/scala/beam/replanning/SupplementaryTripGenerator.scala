package beam.replanning

import beam.agentsim.agents.choice.logit.DestinationChoiceModel.TripParameters.ExpMaxUtility
import beam.agentsim.agents.choice.logit.DestinationChoiceModel._
import beam.agentsim.agents.choice.logit.{DestinationChoiceModel, MultinomialLogit}
import beam.agentsim.agents.choice.mode.ModeChoiceMultinomialLogit
import beam.agentsim.infrastructure.taz.{TAZ, TAZTreeMap}
import beam.router.Modes.BeamMode
import beam.router.Modes.BeamMode.{CAR, CAV, RIDE_HAIL, RIDE_HAIL_POOLED, WALK, WALK_TRANSIT}
import beam.router.skim.SkimsUtils.timeToBin
import beam.router.skim.core.ParkingSkimmer.ChargerType
import beam.sim.BeamServices
import beam.sim.population.AttributesOfIndividual
import org.matsim.api.core.v01.population.{Activity, Leg, Person, Plan}
import org.matsim.api.core.v01.{Coord, Id}
import org.matsim.core.population.PopulationUtils
import org.matsim.utils.objectattributes.attributable.AttributesUtils

import scala.collection.JavaConverters._
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scala.util.Random

class SupplementaryTripGenerator(
  val attributesOfIndividual: AttributesOfIndividual,
  val destinationChoiceModel: DestinationChoiceModel,
  val beamServices: BeamServices,
  val personId: Id[Person],
  val maybeFixedHourOfTripFromODSkims: Option[Int]
) {
  val r: Random.type = scala.util.Random
  val personSpecificSeed: Long = personId.hashCode().toLong

  val travelTimeBufferInSec: Int = 30 * 60

  val activityRates: ActivityRates = destinationChoiceModel.activityRates
  val activityVOTs: ActivityVOTs = destinationChoiceModel.activityVOTs
  val activityDurations: ActivityDurations = destinationChoiceModel.activityDurations
  val modeChoiceCalculator = beamServices.modeChoiceCalculatorFactory(attributesOfIndividual)

  def generateNewPlans(
    plan: Plan,
    destinationChoiceModel: DestinationChoiceModel,
    modes: collection.immutable.Set[BeamMode] = collection.immutable.Set[BeamMode](CAR),
    fillInModes: Boolean = false
  ): Option[Plan] = {
    val modesToConsider: collection.immutable.Set[BeamMode] =
      if (modes.contains(CAV)) {
        collection.immutable.Set[BeamMode](CAV, WALK)
      } else {
        collection.immutable.Set[BeamMode](WALK, WALK_TRANSIT, RIDE_HAIL, RIDE_HAIL_POOLED) ++ modes
      }

    var tourModes = collection.mutable.Set(modesToConsider.toArray: _*)

    val modeMNL: MultinomialLogit[
      BeamMode,
      DestinationChoiceModel.DestinationParameters
    ] =
      MultinomialLogit(
        destinationChoiceModel.DefaultModeParameters,
        destinationChoiceModel.DefaultMNLParameters,
        beamServices.beamConfig.beam.agentsim.agents.tripBehaviors.mulitnomialLogit.mode_nest_scale_factor
      )

    val destinationMNL: MultinomialLogit[
      SupplementaryTripAlternative,
      DestinationChoiceModel.TripParameters
    ] =
      MultinomialLogit(
        Map.empty,
        destinationChoiceModel.TripMNLParameters,
        beamServices.beamConfig.beam.agentsim.agents.tripBehaviors.mulitnomialLogit.destination_nest_scale_factor
      )

    val tripMNL: MultinomialLogit[Boolean, DestinationChoiceModel.TripParameters] =
      MultinomialLogit(
        Map.empty,
        destinationChoiceModel.TripMNLParameters,
        beamServices.beamConfig.beam.agentsim.agents.tripBehaviors.mulitnomialLogit.trip_nest_scale_factor
      )

    val newPlan = PopulationUtils.createPlan(plan.getPerson)
    var anyChanges = false
    newPlan.setType(plan.getType)

    val elements = plan.getPlanElements.asScala.collect { case activity: Activity => activity }.toList

    if (!elements(1).getType.equalsIgnoreCase("temp")) { newPlan.addActivity(elements.head) }

    var updatedPreviousActivity = elements.head

    val activityAccumulator = ListBuffer.empty[Activity]
    val tripAccumulator = ListBuffer.empty[Leg]

    elements.sliding(3).foreach {
      case List(_, curr, next) =>
        if (curr.getType.equalsIgnoreCase("temp")) {
          anyChanges = true
          val (newActivities, newLegs) =
            generateSubtour(
              updatedPreviousActivity,
              curr,
              next,
              modeMNL,
              destinationMNL,
              tripMNL,
              tourModes.toSet,
              fillInModes
            )
          newActivities.foreach { x =>
            activityAccumulator.lastOption match {
              case Some(lastTrip) =>
                if (lastTrip.getType == x.getType) {
                  activityAccumulator -= activityAccumulator.last
                }
              case _ =>
            }
            activityAccumulator.append(x)
          }
          tripAccumulator ++= newLegs

          updatedPreviousActivity = activityAccumulator.last
        } else if (!next.getType.equalsIgnoreCase("temp")) {
          curr.getType match { // If they drove to work they need to drive home
            case "Work" if tourModes.contains(CAR) => tourModes = collection.mutable.Set(CAR)
            case "Work" if tourModes.contains(CAV) => tourModes = collection.mutable.Set(CAV)
            case _                                 =>
          }
          val leg: Leg = generateLeg(curr, next, modeMNL, tourModes.toSet, fillInModes)
          tripAccumulator.append(leg)
          activityAccumulator.append(next)
          updatedPreviousActivity = next
          next.getType match {
            case "Home" =>
              tourModes = collection.mutable.Set(modesToConsider.toArray: _*) // Access to all vehicles at home
            case _ if !leg.getMode.contains("car") => tourModes -= CAR // Can't use car if didn't drive to work
            case _                                 =>
          }
        }
      case _ =>
    }
    activityAccumulator.foreach { x =>
      newPlan.addActivity(x)
    }
    if (!elements(elements.size - 2).getType.equalsIgnoreCase("temp")) { newPlan.addActivity(elements.last) }

    if (anyChanges) {
      //newPlan.setScore(plan.getScore)
      newPlan.setType(plan.getType)
      val resultPlan =
        ReplanningUtil.createPlanFromActivitiesAndTrips(newPlan, activityAccumulator.toVector, tripAccumulator.toVector)
      AttributesUtils.copyAttributesFromTo(plan, resultPlan)
      Some(resultPlan)
    } else {
      None
    }
  }

  private def generateLeg(
    prevActivity: Activity,
    nextActivity: Activity,
    modeMNL: MultinomialLogit[BeamMode, DestinationParameters],
    availableModes: collection.immutable.Set[BeamMode] = collection.immutable.Set[BeamMode](CAR),
    fillInModes: Boolean = false
  ): Leg = {
    if (fillInModes) {
      val modeToTimeAndCost =
        getTazCost(nextActivity, prevActivity, availableModes, bothDirections = false, maybeFixedHourOfTripFromODSkims)
      val alternativeToTimeAndCost = modeToTimeAndCost.map { case (mode, timesAndCost) =>
        val departureTime = prevActivity.getEndTime
        val arrivalTime = timesAndCost.accessTime + departureTime
        val supplementaryTripAlternative: SupplementaryTripAlternative =
          DestinationChoiceModel.SupplementaryTripAlternative(
            TAZ.DefaultTAZ,
            nextActivity.getType,
            mode,
            (arrivalTime - nextActivity.getEndTime).toInt,
            arrivalTime.toInt
          )
        mode -> DestinationChoiceModel.toUtilityParameters(timesAndCost)
      }
      val alternativeChosen = modeMNL.sampleAlternative(alternativeToTimeAndCost, r)
      PopulationUtils.createLeg(alternativeChosen match {
        case Some(alt) => alt.alternativeType.value
        case None      => ""
      })
    } else {
      PopulationUtils.createLeg("")
    }

  }

  private def generateSubtour(
    prevActivity: Activity,
    currentActivity: Activity,
    nextActivity: Activity,
    modeMNL: MultinomialLogit[BeamMode, DestinationParameters],
    destinationMNL: MultinomialLogit[SupplementaryTripAlternative, TripParameters],
    tripMNL: MultinomialLogit[Boolean, TripParameters],
    availableModes: collection.immutable.Set[BeamMode] = collection.immutable.Set[BeamMode](CAR),
    fillInModes: Boolean = false
  ): (List[Activity], List[Leg]) = {
    val tazChoiceSet: List[TAZ] =
      generateTazChoiceSet(
        beamServices.beamConfig.beam.agentsim.agents.tripBehaviors.mulitnomialLogit.max_destination_choice_set_size,
        prevActivity.getCoord
      )
    val alternativeActivity = PopulationUtils.createActivityFromCoord(prevActivity.getType, currentActivity.getCoord)
    alternativeActivity.setStartTime(prevActivity.getStartTime)
    alternativeActivity.setEndTime(nextActivity.getEndTime)
    val (newActivityType, startTime, endTime) = generateSubtourTypeStartAndEndTime(alternativeActivity)
    newActivityType match {
      case "None" => (List(alternativeActivity), List.empty[Leg])
      case _ =>
        val (
          modeTazCosts: Map[SupplementaryTripAlternative, Map[BeamMode, Map[
            DestinationParameters,
            Double
          ]]],
          noTrip: Map[TripParameters, Double]
        ) =
          gatherSubtourCosts(newActivityType, tazChoiceSet, startTime, endTime, alternativeActivity, availableModes)

        val modeChoice: Map[SupplementaryTripAlternative, Map[TripParameters, Double]] =
          modeTazCosts.map { case (alt, modeCost) =>
            val tazMaxUtility = modeMNL.getExpectedMaximumUtility(modeCost)
            alt -> Map[TripParameters, Double](
              TripParameters.ExpMaxUtility -> tazMaxUtility.getOrElse(0)
            )
          }

        val tripMaxUtility = destinationMNL.getExpectedMaximumUtility(modeChoice)

        val tripChoice: Map[Boolean, Map[TripParameters, Double]] =
          Map[Boolean, Map[TripParameters, Double]](
            true -> Map[TripParameters, Double](
              TripParameters.ExpMaxUtility -> tripMaxUtility.getOrElse(0)
            ),
            false -> noTrip
          )

        val tazToChosenMode: Map[TAZ, Option[BeamMode]] = {
          modeTazCosts.map { case (alt, modeCost) =>
            val chosenModeOptionForTaz = modeMNL.sampleAlternative(modeCost, r)
            chosenModeOptionForTaz match {
              case Some(chosenModeForTaz) if fillInModes =>
                alt.taz -> Some(chosenModeForTaz.alternativeType)
              case _ =>
                alt.taz -> None
            }
          }

        }

        val chosenAlternativeOption = tripMNL.sampleAlternative(tripChoice, r) match {
          case Some(mnlSample) if mnlSample.alternativeType => destinationMNL.sampleAlternative(modeChoice, r)
          case _                                            => None
        }

        chosenAlternativeOption match {
          case Some(outcome) =>
            val chosenAlternative = outcome.alternativeType
            val tourModeOption = tazToChosenMode.getOrElse(outcome.alternativeType.taz, None)

            val newActivity =
              PopulationUtils.createActivityFromCoord(
                newActivityType,
                TAZTreeMap.randomLocationInTAZ(chosenAlternative.taz)
              )
            val activityBeforeNewActivity =
              PopulationUtils.createActivityFromCoord(prevActivity.getType, prevActivity.getCoord)
            val activityAfterNewActivity =
              PopulationUtils.createActivityFromCoord(nextActivity.getType, nextActivity.getCoord)

            activityBeforeNewActivity.setStartTime(alternativeActivity.getStartTime)
            activityBeforeNewActivity.setEndTime(startTime - travelTimeBufferInSec)

            newActivity.setStartTime(startTime)
            newActivity.setEndTime(endTime)

            activityAfterNewActivity.setStartTime(endTime + travelTimeBufferInSec)
            activityAfterNewActivity.setEndTime(alternativeActivity.getEndTime)

            val accessLeg = PopulationUtils.createLeg(tourModeOption match {
              case Some(tourMode) => tourMode.value
              case None           => ""
            })

            val egressLeg = PopulationUtils.createLeg(tourModeOption match {
              case Some(tourMode) => tourMode.value
              case None           => ""
            })

            (List(activityBeforeNewActivity, newActivity, activityAfterNewActivity), List(accessLeg, egressLeg))
          case None =>
            (List(alternativeActivity), List.empty[Leg])
        }
    }

  }

  private def gatherSubtourCosts(
    newActivityType: String,
    TAZs: List[TAZ],
    startTime: Int,
    endTime: Int,
    alternativeActivity: Activity,
    modes: collection.immutable.Set[BeamMode]
  ): (
    Map[SupplementaryTripAlternative, Map[BeamMode, Map[DestinationParameters, Double]]],
    Map[TripParameters, Double]
  ) = {
    val alternativeActivityUtility =
      destinationChoiceModel.getActivityUtility(alternativeActivity, attributesOfIndividual)
    val alternativeActivityParamMap = Map[DestinationChoiceModel.TripParameters, Double](
      ExpMaxUtility -> alternativeActivityUtility
    )

    val modeToTazToCost: Map[SupplementaryTripAlternative, Map[BeamMode, Map[DestinationParameters, Double]]] =
      if (TAZs.isEmpty) {
        Map[SupplementaryTripAlternative, Map[BeamMode, Map[DestinationParameters, Double]]]()
      } else {
        TAZs.map { taz =>
          val destinationCoord: Coord = TAZTreeMap.randomLocationInTAZ(taz)
          val additionalActivity = PopulationUtils.createActivityFromCoord(newActivityType, destinationCoord)
          additionalActivity.setStartTime(startTime)
          additionalActivity.setEndTime(endTime)
          val cost =
            getTazCost(
              additionalActivity,
              alternativeActivity,
              modes,
              bothDirections = true,
              maybeFixedHourOfTripFromODSkims
            )
          val alternative =
            DestinationChoiceModel.SupplementaryTripAlternative(
              taz,
              newActivityType,
              CAR,
              endTime - startTime,
              startTime
            )
          alternative -> cost.map { case (x, y) =>
            x -> DestinationChoiceModel.toUtilityParameters(y)
          }
        }.toMap
      }
    (modeToTazToCost, alternativeActivityParamMap)
  }

  private def getRealStartEndTime(
    activity: Activity
  ): (Double, Double) = {
    val start = if (activity.getStartTime > 0) { activity.getStartTime }
    else { 0 }
    val end = if (activity.getEndTime > 0) { activity.getEndTime }
    else { 3600 * 24 }
    (start, end)
  }

  private def getTazCost(
    additionalActivity: Activity,
    alternativeActivity: Activity,
    modes: collection.immutable.Set[BeamMode],
    bothDirections: Boolean = true,
    maybeFixedHourOfTripFromODSkims: Option[Int] = None
  ): Map[BeamMode, DestinationChoiceModel.TimesAndCost] = {
    val (altStart, altEnd) = getRealStartEndTime(alternativeActivity)
    val alternativeActivityDuration = altEnd - altStart
    val activityDuration = additionalActivity.getEndTime - additionalActivity.getStartTime
    val desiredDepartTimeInSeconds = maybeFixedHourOfTripFromODSkims match {
      case Some(fixedHour) => fixedHour * 3600
      case _               => additionalActivity.getStartTime.floor.toInt
    }
    val desiredReturnTimeInSeconds = maybeFixedHourOfTripFromODSkims match {
      case Some(fixedHour) => fixedHour * 3600
      case _               => additionalActivity.getEndTime.floor.toInt
    }
    val vehicleType = beamServices.beamScenario.vehicleTypes.values.head // TODO: FIX WITH REAL VEHICLE
    val fuelPrice = beamServices.beamScenario.fuelTypePrices(vehicleType.primaryFuelType)

    val modeToTimeAndCosts: Map[BeamMode, DestinationChoiceModel.TimesAndCost] =
      modes.map { mode =>
        val accessTripSkim =
          beamServices.skims.od_skimmer.getTimeDistanceAndCost(
            alternativeActivity.getCoord,
            additionalActivity.getCoord,
            desiredDepartTimeInSeconds,
            mode,
            vehicleType.id,
            vehicleType,
            fuelPrice
          )
        val destinationTAZid = beamServices.beamScenario.tazTreeMap
          .getTAZ(additionalActivity.getCoord.getX, additionalActivity.getCoord.getY)
          .tazId
        val parkingSkimOption = beamServices.skims.parking_skimmer.getSkimValue(
          destinationTAZid,
          timeToBin(desiredDepartTimeInSeconds) + accessTripSkim.time,
          ChargerType.NoCharger
        )
        val egressTripSkimOption = if (bothDirections) {
          Some(
            beamServices.skims.od_skimmer.getTimeDistanceAndCost(
              additionalActivity.getCoord,
              alternativeActivity.getCoord,
              desiredReturnTimeInSeconds,
              mode,
              vehicleType.id,
              vehicleType,
              fuelPrice
            )
          )
        } else {
          None
        }

        val parkingCost = destinationChoiceModel.getActivityParkingCost(additionalActivity, parkingSkimOption)

        val startingOverlap =
          (altStart - (additionalActivity.getStartTime - accessTripSkim.time)).max(0)
        val endingOverlap = egressTripSkimOption match {
          case Some(egressTripSkim) => ((additionalActivity.getEndTime + egressTripSkim.time) - altEnd).max(0)
          case None                 => 0.0
        }

        val schedulePenalty = math.pow(startingOverlap, 2) + math.pow(endingOverlap, 2)
        val previousActivityBenefit = egressTripSkimOption match {
          case Some(egressTripSkim) =>
            attributesOfIndividual.getVOT(
              (alternativeActivityDuration - accessTripSkim.time - egressTripSkim.time - activityDuration) / 3600 * activityVOTs
                .getOrElse(alternativeActivity.getType, 1.0)
            )
          case None => 0.0
        }
        val newActivityBenefit: Double =
          destinationChoiceModel.getActivityUtility(additionalActivity, attributesOfIndividual)
        val timesAndCost = egressTripSkimOption match {
          case Some(egressTripSkim) =>
            TimesAndCost(
              accessTripSkim.time,
              egressTripSkim.time,
              -modeChoiceCalculator.utilityOf(mode, accessTripSkim, attributesOfIndividual),
              -modeChoiceCalculator.utilityOf(mode, accessTripSkim, attributesOfIndividual),
              parkingCost,
              schedulePenalty,
              newActivityBenefit + previousActivityBenefit
            )
          case None =>
            TimesAndCost(
              accessTripSkim.time,
              0.0,
              attributesOfIndividual.getVOT(accessTripSkim.generalizedTime / 3600) + accessTripSkim.cost,
              0.0,
              parkingCost,
              schedulePenalty,
              0.0
            )
        }
        mode -> timesAndCost
      }.toMap
    modeToTimeAndCosts
  }

  private def generateSubtourTypeStartAndEndTime(
    alternativeActivity: Activity
  ): (String, Int, Int) = {

    val (altStart, altEnd) = getRealStartEndTime(alternativeActivity)

    val filtered = activityRates.map { case (activityType, hourToRate) =>
      activityType -> hourToRate
        .filter { case (hour, rate) =>
          hour > secondsToIndex(altStart) & hour <= secondsToIndex(altEnd) & rate > 0
        }
        .values
        .sum
    }
    val chosenType = drawKeyByValue(filtered)

    chosenType match {
      case Some(actType) =>
        val meanActivityDuration: Double = activityDurations.getOrElse(actType, 15 * 60)

        val r_repeat = new scala.util.Random
        r_repeat.setSeed(personSpecificSeed)

        val newActivityDuration: Double = -math.log(r_repeat.nextDouble()) * meanActivityDuration

        val earliestPossibleStartIndex = secondsToIndex(altStart + travelTimeBufferInSec)
        val latestPossibleEndIndex = secondsToIndex(altEnd - travelTimeBufferInSec)
        val chosenStartIndex = if (latestPossibleEndIndex > earliestPossibleStartIndex + 1) {
          val filteredRates = activityRates
            .getOrElse(actType, Map[Int, Double]())
            .filter { case (hour, rate) =>
              hour > secondsToIndex(altStart) & hour < secondsToIndex(altEnd - travelTimeBufferInSec) & rate > 0
            }
          drawKeyByValue(filteredRates)
        } else { None }
        chosenStartIndex match {
          case Some(index) =>
            val startTime = math.max((r.nextDouble() + index) * 3600, altStart + travelTimeBufferInSec)
            (
              actType,
              startTime.toInt,
              (startTime + newActivityDuration).toInt
            )
          case None => ("None", 0, 0)
        }
      case None => ("None", 0, 0)
    }
  }

  private def generateTazChoiceSet(
    n: Int,
    coord: Coord
  ): List[TAZ] = {
    val maxDistance =
      beamServices.beamConfig.beam.agentsim.agents.tripBehaviors.mulitnomialLogit.max_destination_distance_meters
    val r_repeat = new scala.util.Random
    val tazToChooseFrom: Seq[TAZ] = {
      val sortedTazInRadius =
        beamServices.beamScenario.tazTreeMap.getTAZInRadius(coord, maxDistance).asScala.toSeq.sortBy(_.tazId.toString)
      sortedTazInRadius
    }

    r_repeat.setSeed(personSpecificSeed)
    r_repeat
      .shuffle(tazToChooseFrom)
      .take(n)
      .toList
  }

  private def secondsToIndex(time: Double): Int = {
    (time / 3600).toInt
  }

  private def drawKeyByValue[A](
    keyToProb: Map[A, Double]
  ): Option[A] = {
    val totalProb = keyToProb.values.sum
    val randomDraw = r.nextDouble()
    val probs = keyToProb.values.scanLeft(0.0)(_ + _ / totalProb).drop(1)
    keyToProb.keys.zip(probs).dropWhile { _._2 <= randomDraw }.headOption match {
      case Some(result) => Some(result._1)
      case _            => None
    }
  }

}
