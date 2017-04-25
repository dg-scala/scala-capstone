package observatory

import java.io.{FileWriter, PrintWriter}

import com.sksamuel.scrimage.Image
import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.{generateTiles, tile}
import observatory.Interaction2.{deviationColors, temperatureColors}
import observatory.Manipulation.{deviation, makeGrid}
import observatory.Visualization2.visualizeGrid

import scala.io.Source
import scala.math._

object Main extends App {

  type Temperatures = Iterable[(Location, Double)]
  type Colors = Iterable[(Double, Color)]
  type Data = (Int) => (Temperatures, Colors)

  val yearTemps = scala.collection.mutable.Map.empty[Int, Temperatures]

  val annualTemperatures: (Colors) => Iterable[(Int, Data)] =
    (cols) => {
      (2000 to 2015).foldRight(Stream.empty[(Int, Data)])((year, stream) => {
        val data =
          (yr: Int) => {
            (yearTemps.get(yr) match {
              case None =>
                val temps: Temperatures = locationYearlyAverageRecords(locateTemperatures(yr, "/stations.csv", s"/$yr.csv"))
                yearTemps.update(yr, temps)
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
    val yearlyData = annualTemperatures(temperatureColors)
    generateTiles[Data](yearlyData, generateImage)
  }

  //  imagine()

  def produceLowerZooms(year: Int, imagesFor: String): Unit = {
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
        val image = Image.fromFile(relativeFile(s"target/$imagesFor/$year/${zoom + 1}/${xPrev}-${yPrev}.png"))
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
      canvas.output(relativeFile(s"target/$imagesFor/$year/$zoom/${x}-${y}.png"))
    }
  }

  def lowerZooms(years: Range, imagesFor: String): Unit =
    for (year <- years) yield produceLowerZooms(year, imagesFor)

  def aggregateYears(): Unit =
    (1975 to 2015).foreach((year) => aggregateYear(year))

  def aggregateYear(year: Int): Unit = {
    val temperatures = locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))
    val fWriter = new PrintWriter(new FileWriter(relativeFile(s"target/averages/$year.csv")), true)
    temperatures.foreach((lt) => fWriter.println(s"${lt._1.lat},${lt._1.lon},${lt._2}"))
    fWriter.close()
  }

  //  aggregateYears()

  def yearAverages(year: Int): Iterable[(Location, Double)] = for {
    line <- Source.fromFile(s"target/averages/$year.csv").getLines.toIterable
  } yield {
    val parts = line.split(",")
    (Location(parts(0).toDouble, parts(1).toDouble), parts(2).toDouble)
  }

  def sparkAverage(temperaturess: Iterable[Iterable[(Location, Double)]]): Iterable[(Location, Double)] = {
    import Extraction.sc
    val rawData = temperaturess
      .foldLeft(sc.emptyRDD[(Location, Double)])((acc, yearData) => acc.union(sc.parallelize(yearData.toSeq)))
    rawData.groupByKey.mapValues((temps) => temps.sum / temps.size).collect.toIterable
  }

  lazy val baseLine = sparkAverage(
    (1975 to 1990)
      .foldLeft(Iterable.empty[Iterable[(Location, Double)]])((acc, year) => acc ++ Iterable(yearAverages(year)))
  )

  type DeviationData = ((Int, Int) => Double, Colors)

  def generateDeviation(year: Int, zoom: Int, x: Int, y: Int, data: ((Int, Int) => Double, Colors)): Unit = {
    val (grid, colors) = (data._1, data._2)
    val img = visualizeGrid(grid, colors, zoom, x, y)
    img.output(relativeFile(s"target/deviations/$year/$zoom/$x-$y.png"))
  }

  def deviate(): Unit = (1990 until 2015).foreach((year) => {
    val temps = yearAverages(year)
    val zoom = 3
    val pairs = for {
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } yield (x, y)

    pairs.par.foreach((xy) =>
      generateDeviation(year, zoom, xy._1, xy._2, (deviation(temps, makeGrid(baseLine)), deviationColors)))
  })

  deviate()

  //  lowerZooms(1990 until 2015, "deviations")
}
