package observatory

import com.sksamuel.scrimage.Image
import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.{generateTiles, tile}

import scala.math._

object Main extends App {

  type Temperatures = Iterable[(Location, Double)]
  type Colors = Iterable[(Double, Color)]
  type Data = (Int) => (Temperatures, Colors)

  var yearTemps = Map.empty[Int, Temperatures]

  val annualTemperatures: (Colors) => Iterable[(Int, Data)] =
    (cols) => {
      (2000 to 2015).foldRight(Stream.empty[(Int, Data)])((year, stream) => {
        val data =
          (yr: Int) => {
            (yearTemps.get(yr) match {
              case None =>
                val temps: Temperatures = locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))
                yearTemps = yearTemps.updated(yr, temps)
                temps

              case Some(x) => x
            }, cols)
          }

        (year, data) #:: stream
      })
    }

  def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: Data): Unit = {
    if (zoom == 3) {
      val (temperatures, colors) = data(year)
      val img = tile(temperatures, colors, zoom, x, y)
      img.output(relativeFile(s"target/temperatures/$year/$zoom/${x}-${y}.png"))
    }
  }

  def imagine(): Unit = {
    val tempColors = Seq(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0))
    )

    val yearlyData = annualTemperatures(tempColors)
    generateTiles[Data](yearlyData, generateImage)
  }

//  imagine()

  def produceLowerZooms(year: Int): Unit = {
    for {
      zoom <- 2 to 0 by -1
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } yield {
      val canvas = Image(256, 256)
      for {
        xPrev <- x * 2 to (x * 2 + 1)
        yPrev <- y * 2 to (y * 2 + 1)
      } yield {
        val image = Image.fromFile(relativeFile(s"target/temperatures/$year/${zoom + 1}/${xPrev}-${yPrev}.png"))
        for {
          col <- 0 until 256 by 2
          row <- 0 until 256 by 2
        } yield {
          val x0 = xPrev - 2 * x
          val y0 = yPrev - 2 * y
          val newCol = x0 * 128 + col / 2
          val newRow = y0 * 128 + row / 2
          assert((0 until 256).indexOf(newCol) != -1, s"newCol out of bounds $newCol, col is $col, x is $x, xPrev is $xPrev")
          assert((0 until 256).indexOf(newRow) != -1, s"newRow out of bounds $newRow, row is $row, y is $y, yPrev is $yPrev")
          canvas.setPixel(newCol, newRow, image.pixel(col, row))
        }
      }
      canvas.output(relativeFile(s"target/temperatures/$year/$zoom/${x}-${y}.png"))
    }
  }

  for {
    year <- 1975 to 2015
  } yield {
    produceLowerZooms(year)
  }
}
