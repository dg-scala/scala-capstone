package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  import Manipulation._

  val temperatures = Seq(
    (Location(45.0, -90.0), 20.0),
    (Location(45.0, 90.0), 0.0),
    (Location(0.0, 0.0), 10.0),
    (Location(-45.0, -90.0), 0.0),
    (Location(-45.0, 90.0), 20.0)
  )

  test("makeGrid returns invalid temperature for invalid locations") {
    val tempGrid = makeGrid(temperatures)

    assert(InvalidTemperature == tempGrid(-90, 0))
    assert(InvalidTemperature == tempGrid(91, 0))
    assert(InvalidTemperature == tempGrid(0, -181))
    assert(InvalidTemperature == tempGrid(0, 180))
  }

  test("makeGrid returns valid temperature for valid locations") {
    val tempGrid = makeGrid(temperatures)
    assert(tempGrid(27, -180).toInt == 10)
  }
}