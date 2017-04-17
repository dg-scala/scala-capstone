package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.zoomedLocation
import observatory.Visualization.{visualizeImage, haversineDistance}


/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x   X coordinate between 0 and 1
    * @param y   Y coordinate between 0 and 1
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
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param zoom   Zoom level of the tile to visualize
    * @param x      X value of the tile to visualize
    * @param y      Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {

    def gridTileTemperature: (Int, Int) => Double = (xCol, yRow) => {
      val loc = zoomedLocation(xCol, yRow, zoom, x, y)

      val tl = Location(loc.lat.ceil, loc.lon.floor)
      val tr = Location(loc.lat.ceil, loc.lon.ceil)
      val bl = Location(loc.lat.floor, loc.lon.floor)
      val br = Location(loc.lat.floor, loc.lon.ceil)

      val d00 = grid(tl.lat.toInt, tl.lon.toInt)
      val d10 = grid(tr.lat.toInt, tr.lon.toInt)
      val d01 = grid(bl.lat.toInt, bl.lon.toInt)
      val d11 = grid(br.lat.toInt, br.lon.toInt)


      val dx = loc.lon - tl.lon
      val dy = tl.lat - loc.lat
      bilinearInterpolation(dx, dy, d00, d01, d10, d11)
    }

    val canvas = Image(256, 256)
    val alpha = 127
    visualizeImage(canvas, colors, alpha)(gridTileTemperature)
    canvas
  }

}
