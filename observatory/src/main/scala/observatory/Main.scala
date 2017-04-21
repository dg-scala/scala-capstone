package observatory

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.{generateTiles, tile}

object Main extends App {

  type Temperatures = Iterable[(Location, Double)]
  type Colors = Iterable[(Double, Color)]
  type Data = (Int) => (Temperatures, Colors)

  val yearTemps = scala.collection.mutable.Map.empty[Int, Temperatures]

  val annualTemperatures: (Colors) => Iterable[(Int, Data)] =
    (cols) => {
      (1977 to 2015).foldRight(Stream.empty[(Int, Data)])((year, stream) => {
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

  imagine()

}
