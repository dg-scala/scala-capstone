package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  import Extraction._

  test("empty sequence returned when stations file not found") {

    val actual = locateTemperatures(1975, "/dummy.csv", "/1975.csv")
    assert(actual == Iterable.empty[(LocalDate, Location, Double)], "empty sequence expected for invalid station files")
  }

  test("emtpy sequence returned when temperatures file not found") {

    val actual = locateTemperatures(1975, "/stations.csv", "/1973.csv")
    assert(actual == Iterable.empty[(LocalDate, Location, Double)], "empty sequence expected for invalid temperatures files")
  }

  test("empty sequence returned for temperatures with no location coordinates") {

    val actual = locateTemperatures(1975, "/stations.csv", "/no_location.csv")
    assert(actual == Iterable.empty[(LocalDate, Location, Double)], "empty sequence expected for temperatures with no location")
  }

  test("correct temperatures returned for test data") {
    val expected = Seq(
      (LocalDate.of(1975, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(1975, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(1975, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val actual =
      locateTemperatures(1975, "/stations.csv", "/1975.csv")
        .map(r => (r._1, r._2, Math.round(r._3 * 10.0).toDouble / 10.0))
    assert(actual.forall(a => expected.contains(a)), "correct temperatures should be returned for test data")
  }

  test("correct averages temperatures returned for test data") {
    val expected = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    val actual =
      locationYearlyAverageRecords(locateTemperatures(1975, "/stations.csv", "/1975.csv"))
        .map(r => (r._1, Math.round(r._2 * 10.0).toDouble / 10.0))
    assert(actual.forall(a => expected.contains(a)), "correct averages temperatures should be returned for test data")
  }

}