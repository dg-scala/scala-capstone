package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
import Visualization._

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
    val canvas = Image(256, 256)
    val alpha = 127
    visualizeImage(canvas, colors, alpha)((xCol, yRow) => {
      predictTemperature(temperatures, zoomedLocation(xCol, yRow, zoom, x, y))
    })
    canvas
  }

  /**
    * @param xCol  Column coordinate of the pixel
    * @param yRow  Row coordinate of the pixel
    * @param zoom  Tile zoom
    * @param xTile Tile X coordinate
    * @param yTile Tile Y coordinate
    * @return The `Location` of a pixel in a tile defined by `x`, `y` and `zoom`
    */
  def zoomedLocation(xCol: Int, yRow: Int, zoom: Int, xTile: Int, yTile: Int): Location =
    tileLocation(zoom + 8, xTile * 256 + xCol, yTile * 256 + yRow)

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
    yearlyData.foreach((yd) => {
      val year = yd._1
      val data = yd._2
      for {
        zoom <- 0 to 3
      } yield {
        val tileCoordinates = for {
          x <- 0 until pow(2, zoom).toInt
          y <- 0 until pow(2, zoom).toInt
        } yield (x, y)

        tileCoordinates.par.foreach((xy) => generateImage(year, zoom, xy._1, xy._2, data))
      }
    })
  }

}
