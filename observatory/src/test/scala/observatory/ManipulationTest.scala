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

  val temperatures1 = Seq(
    (Location(45.0, -90.0), 21.0),
    (Location(45.0, 90.0), 1.0),
    (Location(0.0, 0.0), 11.0),
    (Location(-45.0, -90.0), 1.0),
    (Location(-45.0, 90.0), 21.0)
  )

  val temperatures2 = Seq(
    (Location(45.0, -90.0), 22.0),
    (Location(45.0, 90.0), 2.0),
    (Location(0.0, 0.0), 12.0),
    (Location(-45.0, -90.0), 2.0),
    (Location(-45.0, 90.0), 22.0)
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

  test("averages returns valid temperature for valid locations") {
    val averageTemperature = average(Seq(temperatures, temperatures1, temperatures2))
    assert(averageTemperature(27, -180).toInt == 11)
  }

  test("grader failed test for average") {
    val temperaturess =
      Seq(
        Seq(
          (Location(45.0, -90.0), 2.0),
          (Location(0.0, 90.0), 20.0)
        ),
        Seq(
          (Location(0.0, -90.0), 11.0),
          (Location(45.0, 90.0), 14.0)
        )
      )
    val avgTemp = average(temperaturess)
    assert(avgTemp(89, -179) == 9.498511019098709)
  }

}