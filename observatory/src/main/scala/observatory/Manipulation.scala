package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  import Visualization._

  val InvalidTemperature: Double = 9999.9

  case class TemperatureAtLocation(lat: Double, lon: Double, temperature: Double)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double =
    (lat, lon) => {
      if (lat < -89 || lat > 90 || lon < (-180) || lon > 179)
        InvalidTemperature
      else {
        predictTemperature(temperatures, Location(lat, lon))
      }
    }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = (lat, lon) => {
    val averages =
      temperaturess.foldLeft(Iterable.empty[Double])((acc, temps) => acc ++ Iterable(makeGrid(temps)(lat, lon)))
    averages.sum / averages.size
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double =
    (lat, lon) => {
      val normalTemp = normals(lat, lon)
      val currentTemp = makeGrid(temperatures)(lat, lon)
      currentTemp - normalTemp
    }


}

