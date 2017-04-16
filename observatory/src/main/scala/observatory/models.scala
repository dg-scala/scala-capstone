package observatory

case class Location(lat: Double, lon: Double) {
  import math._

  private val latRadians = toRadians(lat)
  private val lonRadians = toRadians(lon)

  val sinLat: Double = sin(latRadians)
  val cosLat: Double = cos(latRadians)
  val sinLon: Double = sin(lonRadians)
  val cosLon: Double = cos(lonRadians)
}

case class Color(red: Int, green: Int, blue: Int)

