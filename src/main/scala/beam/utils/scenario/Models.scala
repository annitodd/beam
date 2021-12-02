package beam.utils.scenario

case class PersonId(id: String) extends AnyVal

case class HouseholdId(id: String) extends AnyVal

case class PersonInfo(
  personId: PersonId,
  householdId: HouseholdId,
  rank: Int,
  age: Int,
  student: Int,
  excludedModes: Seq[String] = Seq.empty,
  isFemale: Boolean,
  valueOfTime: Double
)

object PlanElement {
  sealed trait PlanElementType

  object PlanElementType {

    def apply(planElementType: String): PlanElementType =
      planElementType match {
        case "activity" => Activity
        case "leg"      => Leg
      }
  }

  case object Activity extends PlanElementType {
    override def toString: String = "activity"
  }

  case object Leg extends PlanElementType {
    override def toString: String = "leg"
  }
}

case class PlanElement(
  personId: PersonId,
  planIndex: Int,
  planScore: Double,
  planSelected: Boolean,
  planElementType: PlanElement.PlanElementType,
  planElementIndex: Int,
  activityType: Option[String],
  activityLocationX: Option[Double],
  activityLocationY: Option[Double],
  activityEndTime: Option[Double],
  legMode: Option[String],
  legDepartureTime: Option[String],
  legTravelTime: Option[String],
  legRouteType: Option[String],
  legRouteStartLink: Option[String],
  legRouteEndLink: Option[String],
  legRouteTravelTime: Option[Double],
  legRouteDistance: Option[Double],
  legRouteLinks: Seq[String],
  geoId: Option[String]
)

case class HouseholdInfo(householdId: HouseholdId, cars: Int, income: Double, locationX: Double, locationY: Double)

case class VehicleInfo(vehicleId: String, vehicleTypeId: String, householdId: String)
