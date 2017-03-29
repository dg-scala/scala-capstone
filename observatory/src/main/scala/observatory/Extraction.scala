package observatory

import java.time.LocalDate

import scala.tools.nsc.interpreter.InputStream

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = getClass.getClassLoader.getResourceAsStream(stationsFile)
    val temperatures = getClass.getClassLoader.getResourceAsStream(temperaturesFile)

    loadTemperaturesFromStreams(stations, temperatures)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

  def loadTemperaturesFromStreams(stations: InputStream, temperatures: InputStream): Iterable[(LocalDate, Location, Double)] = {
    if (stations == null || temperatures == null)
      Iterable.empty[(LocalDate, Location, Double)]
    else
      throw new Error("Not implemented")
  }

}
