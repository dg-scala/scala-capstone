package observatory

case class Location(lat: Double, lon: Double) {
  import scala.math._
  private val latRad = lat.toRadians
  private val lonRad = lon.toRadians

  val sinLat: Double = sin(latRad)
  val sinLon: Double = sin(lonRad)
  val cosLat: Double = cos(latRad)
  val cosLon: Double = cos(lonRad)
}

case class Color(red: Int, green: Int, blue: Int)

