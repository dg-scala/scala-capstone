package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import monix.eval.Coeval.Error

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

    if (temperatures.isEmpty) throw new RuntimeException("temperatures cannot be empty")

    val (numerator, denominator, exact) =
      temperatures.foldLeft((0.0, 0.0, 9999.9))((acc, loctemp) => {
        val (l, t) = (loctemp._1, loctemp._2)
        val (n, d, e) = (acc._1, acc._2, acc._3)
        val distance = haversineDistance(l, location)
//        val distance = transformedHaversineDistance(l, location)

        if (e != 9999.9) acc
        else if (distance == 0) (n, d, t) // known temperature
        else (n + weight(distance, powerForWeights) * t, d + weight(distance, powerForWeights), e)
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

    if (points.isEmpty) throw new RuntimeException("points cannot be empty")

    def closestPoints: ((Double, Color), (Double, Color)) = {
      val pointsMap = points.toMap
      val (low, high) =
        pointsMap.keys.foldLeft((Double.MinValue, Double.MaxValue))((lh, pt) => {
          val (lo, hi) = (lh._1, lh._2)

          if (lo == value && hi == value) (lo, hi)
          else if (pt == value) (pt, pt)
          else if (pt < value) {
            if (pt > lo) (pt, hi)
            else (lo, hi)
          }
          else {
            if (pt < hi) (lo, pt)
            else (lo, hi)
          }
        })

      if (low == Double.MinValue) ((high, pointsMap(high)), (high, pointsMap(high)))
      else if (high == Double.MaxValue) ((low, pointsMap(low)), (low, pointsMap(low)))
      else ((low, pointsMap(low)), (high, pointsMap(high)))
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
    val canvas = Image(360, 180)
    val alpha: Int = 255
    visualizeImage(canvas, colors, alpha)((x, y) => {
      predictTemperature(temperatures, coordinatesToLocation(x, y))
    })
    canvas
  }

  def visualizeImage(canvas: Image, colors: Iterable[(Double, Color)], alpha: Int)(estimateTemperature: (Int, Int) => Double): Unit = {
    canvas.points.par.foreach((xy) => {
      val (x, y) = (xy._1, xy._2)
      val temp = estimateTemperature(x, y)
      val xyColor = interpolateColor(colors, temp)
      canvas.setPixel(x, y, Pixel(xyColor.red, xyColor.green, xyColor.blue, alpha))
    })
  }

  // the higher the fudgeFactor the more closer geolocations dominate predictTemperatures calculation
  val powerForWeights = 2.0

  def weight(distance: Double, power: Double): Double = 1 / pow(distance, power)

  val earthRadius = 6372.8 //radius in km

  // method adapted from https://rosettacode.org/wiki/Haversine_formula#Scala
  def haversineDistance(l1: Location, l2: Location): Double = {
    val (lat1, lon1) = (l1.lat, l1.lon)
    val (lat2, lon2) = (l2.lat, l2.lon)
    val dLat = (lat2 - lat1).toRadians
    val dLon = (lon2 - lon1).toRadians

    val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1.toRadians) * cos(lat2.toRadians)
    val c = 2 * arcsin(sqrt(a))
    earthRadius * c
  }

//  def transformedHaversineDistance(l1: Location, l2: Location): Double = {
//    val cosDlat = l1.cosLat * l2.cosLat + l1.sinLat * l2.sinLat
//    val cosDlon = l1.cosLon * l2.cosLon + l1.sinLon * l2.sinLon
//    val sinHalfDlatSq = (1 - cosDlat) / 2
//    val sinHalfDlonSq = (1 - cosDlon) / 2
//
//    val a = sinHalfDlatSq + sinHalfDlonSq * l1.cosLat * l2.cosLat
//    val c = 2 * arcsin(sqrt(a))
//    earthRadius * c
//  }

  def arcsin(angle: Double): Double = {
    val negate = if (angle < 0.0) 1.0 else 0.0
    val x = abs(angle)
    var ret = -0.0187293
    ret *= x
    ret += 0.0742610
    ret *= x
    ret -= 0.2121144
    ret *= x
    ret += 1.5707288
    ret = Pi * 0.5 - sqrt(1.0 - x) * ret
    ret - 2 * negate * ret
  }

  def interpolate2(before: (Double, Color), after: (Double, Color), value: Double): Color = {
    def interpolateComponent(y0: Int, y1: Int): Int =
      round(y0.toDouble + (value - before._1) * (y1.toDouble - y0.toDouble) / (after._1 - before._1)).toInt

    Color(
      interpolateComponent(before._2.red, after._2.red),
      interpolateComponent(before._2.green, after._2.green),
      interpolateComponent(before._2.blue, after._2.blue)
    )
  }

  def coordinatesToLocation(x: Int, y: Int) = Location(90 - y, x - 180)

}

