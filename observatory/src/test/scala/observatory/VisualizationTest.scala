package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  import Visualization._

  val tolerance = earthRadius / 100.0 // 1% of Earth's radius

  def approxEq(actual: Double, expected: Double, eta: Double): Boolean =
    ((expected - eta) <= actual) && (actual <= (expected + eta))

  def between(actual: Double, x: Double, y: Double): Boolean =
    (x <= actual) && (actual <= y)

  test("arial distance between London and Belgrade") {
    val expected: Double = 1688.97
    val locLondon = Location(51.5000, -0.1167)
    val locBelgrade = Location(44.8186, 20.4681)
    val actual = haversineDistance(locLondon, locBelgrade)
    assert(approxEq(actual, expected, tolerance),
      s"arial distance betwen LON and BEG should be accurate to witin 10.0km, actual=$actual expected=$expected")
  }

  test("arial distance between London and Auckland") {
    val expected: Double = 18324.61
    val locLondon = Location(51.5000, -0.1167)
    val locAuckland = Location(-36.8667, 174.7667)
    val actual = haversineDistance(locLondon, locAuckland)
    assert(approxEq(actual, expected, tolerance),
      s"arial distance betwen LON and ACK should be accurate to witin 10.0km, actual=$actual expected=$expected")
  }

  test("arial distance between Paris and Nairobi") {
    val expected: Double = 6484.05
    val locParis = Location(48.8534, 2.3488)
    val locNairobi = Location(-1.28333, 36.8167)
    val actual = haversineDistance(locParis, locNairobi)
    assert(approxEq(actual, expected, tolerance),
      s"arial distance betwen PAR and NBO should be accurate to witin 10.0km, actual=$actual expected=$expected")
  }

  test("predict temperature test") {
    val temps = Seq(
      (Location(51.5000, -0.1167), 17.5),
      (Location(44.8186, 20.4681), 27.5),
      (Location(-36.8667, 174.7667), 10.5),
      (Location(48.8534, 2.3488), 18.5),
      (Location(-1.28333, 36.8167), 40.5)
    )
    val location = Location(49.95, 1.25)
    val temp = predictTemperature(temps, location)
    println(s"temp = $temp")
    assert(between(temp, 17.5, 18.5), "temperature should be closer to the closest points")
  }

}
