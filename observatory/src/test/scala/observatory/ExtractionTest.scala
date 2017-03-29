package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  import Extraction._

  test("empty sequence returned when stations file not found") {

    val actual = locateTemperatures(1975, "dummy.txt", "1975.txt")
    assert(actual == Iterable.empty[(LocalDate, Location, Double)], "empty sequence expected for invalid station files")
  }

  test("emtpy sequence returned when temperatures file not found") {

    val actual = locateTemperatures(1975, "stations.txt", "1973.txt")
    assert(actual == Iterable.empty[(LocalDate, Location, Double)], "empty sequence expected for invalid temperatures files")
  }

  test("empty sequence returned for temperatures with no location coordinates") {
    val actual = loadTemperaturesFromStreams(stations, temps)
    assert(actual == Iterable.empty[(LocalDate, Location, Double)], "empty sequence expected for temperatures without locations"))
  }
}