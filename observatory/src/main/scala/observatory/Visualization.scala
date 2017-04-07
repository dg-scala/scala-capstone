package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    if (temperatures.isEmpty)
      throw new Error("You cannot predictTemperature with an empty temperatures data set.")

    val (numerator, denominator, exact) =
      temperatures.foldLeft((0.0, 0.0, 9999.9))((acc, loctemp) => {
        val (l, t) = (loctemp._1, loctemp._2)
        val (n, d, e) = (acc._1, acc._2, acc._3)

        if (e != 9999.9) acc
        else if (haversineDistance(l, location) == 0) (n, d, t) // known temperature
        else (n + weight(l, location, powerForWeights) * t, d + weight(l, location, powerForWeights), e)
      })

    if (exact != 9999.9) exact
    else numerator / denominator
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    def closestPoints: ((Double, Color), (Double, Color)) = {
      val pointsMap = points.toMap
      val (low, high) =
        pointsMap.keys.foldLeft((Double.MinValue, Double.MaxValue))((lh, pt) => {
          val (l, h) = (lh._1, lh._2)

          if (l == value && h == value) (l, h)
          else if (pt == value) (pt, pt)
          else if (pt < value) {
            if (pt > l) (pt, h)
            else (l, h)
          }
          else {
            if (pt < h) (l, pt)
            else (l, h)
          }
        })

      ((low, pointsMap(low)), (high, pointsMap(high)))
    }

    val (before, after) = (closestPoints._1._1, closestPoints._2._1)
    val (bCol, aCol) = (closestPoints._1._2, closestPoints._2._2)

    if (before == value && value == after) bCol // exact point
    else if (value < before) bCol // the lower bound
    else if (value > after) aCol // the upper bound
    else interpolate2(closestPoints._1, closestPoints._2, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

  // the higher the fudgeFactor the more closer geolocations dominate predictTemperatures calculation
  val powerForWeights = 2.0

  def weight(l1: Location, l2: Location, power: Double): Double = 1 / pow(haversineDistance(l1, l2), power)

  val earthRadius = 6372.8 //radius in km

  // method adapted from https://rosettacode.org/wiki/Haversine_formula#Scala
  def haversineDistance(l1: Location, l2: Location): Double = {
    val (lat1, lon1) = (l1.lat, l1.lon)
    val (lat2, lon2) = (l2.lat, l2.lon)
    val dLat = (lat2 - lat1).toRadians
    val dLon = (lon2 - lon1).toRadians

    val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1.toRadians) * cos(lat2.toRadians)
    val c = 2 * asin(sqrt(a))
    earthRadius * c
  }

  def interpolate2(before: (Double, Color), after: (Double, Color), value: Double): Color = {
    def interpolateComponent(y0: Int, y1: Int): Int =
      (y0 + (value - before._1) * (y1 - y0) / (after._1 - before._1)).toInt

    Color(
      interpolateComponent(before._2.red, after._2.red),
      interpolateComponent(before._2.green, after._2.green),
      interpolateComponent(before._2.blue, after._2.blue)
    )
  }

}

