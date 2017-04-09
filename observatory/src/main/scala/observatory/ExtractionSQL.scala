package observatory

import java.io.InputStream
import java.net.URL
import java.nio.file.Paths

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Dataset

/**
  * Created by dragan on 09/04/17.
  */
object ExtractionSQL {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Capstone")
      .config("spark.master", "local")
      .getOrCreate()

  import spark.implicits._

  case class StationID(stn: String, wban: String)

  case class Station(stationId: StationID, latitude: Double, longitude: Double)

  case class TemperatureRecord(stationId: StationID, month: Int, day: Int, fahrenheit: Double)

  case class TemperatureAverage(stationId: StationID, celsius: Double)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecordsSQL(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(Location, Double)] = {
    def sequenceFromStream(is: InputStream): Seq[String] = scala.io.Source.fromInputStream(is).getLines().toSeq

    val stationsStream = getClass.getResourceAsStream(stationsFile)
    val temperaturesStream = getClass.getResourceAsStream(temperaturesFile)

    val tempDS = temperatures(spark.sparkContext.parallelize(sequenceFromStream(temperaturesStream)))
    val statDS = stations(spark.sparkContext.parallelize(sequenceFromStream(stationsStream)))

    val averages = tempDS
      .groupByKey(row => row.stationId)
      .agg(avg($"fahrenheit").as[Double])
      .map(row => TemperatureAverage(row._1, celsius(row._2)))

    averages.join(statDS, averages("stationId") === statDS("stationId"))
      .map(row => (
        Location(
          row(3).asInstanceOf[Double],
          row(4).asInstanceOf[Double]),
        row(1).asInstanceOf[Double]))
      .collect
      .toIterable
  }

  def celsius(fahrenheit: Double): Double = (fahrenheit - 32.0) * 5 / 9

  def fsPath(pathURL: URL): String = Paths.get(pathURL.toURI).toString

  def stations(rawStations: RDD[String]): Dataset[Station] = {

    def invalidStation(record: Array[String]) =
      if (record.length != 4)
        true
      else {
        val (stn, wban, lat, long) = (record(0), record(1), record(2), record(3))
        (stn.isEmpty && wban.isEmpty) || lat.isEmpty || long.isEmpty
      }

    val rdd = rawStations
      .map(line => line.split(","))
      .filter(stationRecord => !invalidStation(stationRecord))
      .map(s => Station(StationID(s(0), s(1)), s(2).toDouble, s(3).toDouble))
      .cache()

    spark.createDataset(rdd)
  }

  def temperatures(rawTemperatures: RDD[String]): Dataset[TemperatureRecord] = {

    def invalidTemperatureRecord(record: Array[String]) = record.length != 5 || record(4) == "9999.9"

    val rdd = rawTemperatures
      .map(line => line.split(","))
      .filter(tempRecord => !invalidTemperatureRecord(tempRecord))
      .map(t => TemperatureRecord(StationID(t(0), t(1)), t(2).toInt, t(3).toInt, t(4).toDouble))
      .cache()

    spark.createDataset(rdd)
  }

}
