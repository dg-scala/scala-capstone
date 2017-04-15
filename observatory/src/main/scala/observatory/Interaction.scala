package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n: Double = pow(2.0, zoom)
    val lon = (x.toDouble * 360 / n) - 180
    val lat = toDegrees(atan(sinh(Pi * (1 - 2 * y.toDouble / n))))
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zoom`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    def debug(): Unit = {
      println("[DRAGAN] Start")
      println(s"zoom = $zoom, x = $x, y = $y")
      println("-- colors --")
      colors.foreach((dc) => println(s"temp = ${dc._1}, col = ${dc._2}"))
      println("-- temperatures --")
      temperatures.foreach((ld) => println(s"loc = ${ld._1}, temp = ${ld._2}"))
      println("[DRAGAN] End")
    }
//    debug()

    import Visualization._

    val canvas = Image(256, 256)
    val alpha = 255

    /**
      *  @param xCol  Column coordinate of the pixel
      *  @param yRow  Row coordinate of the pixel
      *  @return The `Location` of a pixel in a tile defined by `x`, `y` and `zoom`
      */
    def zoomedLocation(xCol: Int, yRow: Int): Location = {
      val topLeft = tileLocation(zoom, x, y)
      val bottomRight = tileLocation(zoom, x + 1, y + 1)

      val deltaY = bottomRight.lat - topLeft.lat
      val deltaX = bottomRight.lon - topLeft.lon
      Location(
        topLeft.lat + (yRow * deltaY / 256),
        topLeft.lon + (xCol * deltaX / 256)
      )
    }

    visualizeImage(canvas, temperatures, colors, alpha)(zoomedLocation)
    canvas
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    ???
  }

}
