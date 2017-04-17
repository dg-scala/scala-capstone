package observatory

import com.sksamuel.scrimage.Image

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val canvas = Image(256,256)
    val alpha = 127

    canvas.points.par.foreach((xy) => {
      // work out geolocation for lat and lon, given zoom and x and y
      // interpolate temperature for lat and lon
      // interpolate colour for lat and lon
      ???
    })
    canvas
  }

//  def visualizeImage(canvas: Image, temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], alpha: Int)
//    (xy2Location: (Int, Int) => Location): Unit = {
//    canvas.points.par.foreach((xy) => {
//      val (x, y) = (xy._1, xy._2)
//      val temp = predictTemperature(temperatures, xy2Location(x, y))
//      val xyColor = interpolateColor(colors, temp)
//      canvas.setPixel(x, y, Pixel(xyColor.red, xyColor.green, xyColor.blue, alpha))
//    })
//  }

}
