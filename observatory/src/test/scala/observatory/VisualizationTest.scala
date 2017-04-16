package observatory


import com.sksamuel.scrimage.Image
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.util.Try

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  import Visualization._

  val tolerance = earthRadius / 100.0 // 1% of Earth's radius

  def approxEq(actual: Double, expected: Double, eta: Double): Boolean =
    ((expected - eta) <= actual) && (actual <= (expected + eta))

  def between(actual: Double, x: Double, y: Double): Boolean =
    (x <= actual) && (actual <= y)

  val tempColors = Seq(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

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
    assert(between(temp, 17.5, 18.5), "temperature should be closer to the closest points")
  }

  test("test empty points") {
    import scala.util.control.Exception._

    val empty = Iterable.empty[(Double, Color)]
    val actual: Try[Color] = catching(classOf[RuntimeException]).withTry(interpolateColor(empty, 0.0))
    assert(actual.isFailure, "interpolateColor should throw RuntimeException for empty points")
  }

  test("interpolate colors with exact match") {
    val expected = Color(0, 255, 255)
    val actual = interpolateColor(tempColors, 0.0)
    assert(actual == expected, "interpolate colors should return exact color for known temperature")
  }

  test("interpolate should respect upper and lower bounds") {
    val upper = Color(255, 255, 255)
    val lower = Color(0, 0, 0)
    assert(upper == interpolateColor(tempColors, 70.0), "interpolate should respect upper bound")
    assert(lower == interpolateColor(tempColors, -61.0), "interpolate should respect lower bound")
  }

  test("interpolate color in range but not exact") {
    val expected = Color(255, 128, 0)
    val actual = interpolateColor(tempColors, 22.0)
    assert(actual == expected, "color should be correctly interpolated for unknown temperature in range")
  }

  test("grader [#2 - Raw data display] color interpolation") {
    val expected = Color(128,0,128)
    val actual = interpolateColor(List((-2.147483648E9,Color(255,0,0)), (-1.0,Color(0,0,255))), -1.0737418245E9)
    assert(actual == expected, s"Failed grader [#2 - Raw data display] color interpolation, $expected, $actual")
  }

  test("transform coordinates to location") {
    assert(Location(90, -180) == coordinatesToLocation(0, 0))
    assert(Location(0.0, 0.0) == coordinatesToLocation(180, 90))
    assert(Location(-90, 180) == coordinatesToLocation(360, 180))
    assert(Location(-90, -180) == coordinatesToLocation(0, 180))
    assert(Location(90, 180) == coordinatesToLocation(360, 0))
  }

  test("visualize this") {
    val ts = List((Location(45.0,-90.0),36.1643835403579), (Location(-45.0,0.0),-21.219052136494582))
    val cols = List((36.1643835403579,Color(255,0,0)), (-21.219052136494582,Color(0,0,255)))
    val t = visualize(ts, cols)
//    t.output(Paths.get(getClass.getResource("/visualiseThis.png").toURI))
    assert(imageEq(t, Image.fromResource("/visualiseThis.png")))
  }

}
