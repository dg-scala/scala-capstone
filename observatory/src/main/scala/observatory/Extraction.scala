package observatory

import java.net.URL
import java.nio.file.Paths
import java.time.LocalDate
import java.io.InputStream

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel


/**
  * 1st milestone: data extraction
  */
object Extraction {

  case class StationID(stn: String, wban: String)

  case class Station(stationId: StationID, latitude: Double, longitude: Double)

  case class TemperatureRecord(stationId: StationID, month: Int, day: Int, fahrenheit: Double)


  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Capstone")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    def sequenceFromStream(is: InputStream): Seq[String] = scala.io.Source.fromInputStream(is).getLines().toSeq

    val stationsStream = getClass.getResourceAsStream(stationsFile)
    val temperaturesStream = getClass.getResourceAsStream(temperaturesFile)

    if (stationsStream == null || temperaturesStream == null)
      Iterable.empty[(LocalDate, Location, Double)]
    else {
      val statRdd = stations(sc.parallelize(sequenceFromStream(stationsStream)))
      val tempRdd = temperatures(sc.parallelize(sequenceFromStream(temperaturesStream)))

      val resultsRdd =
        statRdd.join(tempRdd)
          .mapValues(value => {
            val station: Station = value._1
            val tempRecords: Iterable[TemperatureRecord] = value._2

            val location = Location(station.latitude, station.longitude)
            tempRecords.map(tempRecord =>
              (LocalDate.of(year, tempRecord.month, tempRecord.day), location, celsius(tempRecord.fahrenheit))
            )
          })
          .values

      if (resultsRdd.isEmpty())
        Iterable.empty[(LocalDate, Location, Double)]
      else
        resultsRdd.reduce((v1, v2) => v1 ++ v2)
    }
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val recordsRdd = sc.parallelize(records.toSeq)

    recordsRdd
      .groupBy(_._2)
      .mapValues(records => records.foldRight(0.0)((rec, sum) => sum + rec._3) / records.size)
      .aggregate(Iterable.empty[(Location, Double)])(
        (acc, ld) => acc ++ Iterable.apply(ld),
        (a1, a2) => a1 ++ a2
      )
  }

  def celsius(fahrenheit: Double): Double = (fahrenheit - 32.0) * 5 / 9

  def fsPath(pathURL: URL): String = Paths.get(pathURL.toURI).toString

  def stations(rawStations: RDD[String]): RDD[(StationID, Station)] = {

    def invalidStation(record: Array[String]) =
      if (record.length != 4)
        true
      else {
        val (stn, wban, lat, long) = (record(0), record(1), record(2), record(3))
        (stn.isEmpty && wban.isEmpty) || lat.isEmpty || long.isEmpty
      }

    rawStations
      .map(line => line.split(","))
      .filter(stationRecord => !invalidStation(stationRecord))
      .map(s => Station(StationID(s(0), s(1)), s(2).toDouble, s(3).toDouble))
      .groupBy(_.stationId)
      .mapValues(_.head)
      .persist(StorageLevel.MEMORY_AND_DISK)
  }

  def temperatures(rawTemperatures: RDD[String]): RDD[(StationID, Iterable[TemperatureRecord])] = {

    def invalidTemperatureRecord(record: Array[String]) = record.length != 5 || record(4) == "9999.9"

    rawTemperatures
      .map(line => line.split(","))
      .filter(tempRecord => !invalidTemperatureRecord(tempRecord))
      .map(t => TemperatureRecord(StationID(t(0), t(1)), t(2).toInt, t(3).toInt, t(4).toDouble))
      .groupBy(_.stationId)
      .persist(StorageLevel.MEMORY_AND_DISK)
  }
}
