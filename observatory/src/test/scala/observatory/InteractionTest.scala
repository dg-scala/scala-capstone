package observatory

import java.nio.file.Paths

import com.sksamuel.scrimage.Image
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  import Interaction._

  val temperatures = Seq(
    (Location(45.0, -90.0), 20.0),
    (Location(45.0, 90.0), 0.0),
    (Location(0.0, 0.0), 10.0),
    (Location(-45.0, -90.0), 0.0),
    (Location(-45.0, 90.0), 20.0)
  )

  val colors = Seq(
    (0.0, Color(255,0,0)),
    (10.0, Color(0,255,0)),
    (20.0, Color(0,0,255))
  )

  def imageEq(im1: Image, im2: Image): Boolean = im1.pixels.sameElements(im2.pixels)

  test("test grader rubbish") {
    import Visualization._
    val temp = predictTemperature(temperatures, Location(-27.059125784374057,-180.0))
    val col = interpolateColor(colors, temp)
    val xCol = 0.0 // topLeft.lon + (xCol * deltaX / 256); topLeft.lon = -180.0, brCorner.lon = 180.0
    val yRow = 168.723364287931977 // topLeft.lat + (yRow * deltaY / 256); topLeft.lat = 85.05112877980659, brCorner.lat = -85.05112877980659
    assert(temp == 10.0)
    assert(col == Color(0, 255, 0))
  }

  test("tileLocation for various zooms") {
    val l000 = tileLocation(0, 0, 0)
    val l011 = tileLocation(0, 1, 1)
    assert(l000 == Location(85.05112877980659,-180.0))
    assert(l011 == Location(-85.05112877980659,180.0))

    val l1tl = tileLocation(1, 0, 0)
    val l1bl = tileLocation(1, 0, 1)
    val l1tr = tileLocation(1, 1, 0)
    val l1br = tileLocation(1, 1, 1)
    val l122 = tileLocation(1, 2, 2)
    assert(l1tl == Location(85.05112877980659,-180.0))
    assert(l1bl == Location(0.0,-180.0))
    assert(l1tr == Location(85.05112877980659,0.0))
    assert(l1br == Location(0.0,0.0))
    assert(l122 == Location(-85.05112877980659,180.0))

    val l2tl = tileLocation(2, 0, 0)
    assert(l2tl == l1tl)
    val l202 = tileLocation(2, 0, 2)
    assert(l202 == l1bl)
    val l220 = tileLocation(2, 2, 0)
    assert(l220 == l1tr)
    val l2mid = tileLocation(2, 2, 2)
    assert(l2mid == l1br)
    val l244 = tileLocation(2, 4, 4)
    assert(l244 == l122)
  }

  test("tile on zoom 1") {
    val tile0 = tile(temperatures, colors, 0, 0, 0)
//    tile0.output(Paths.get(getClass.getResource("/tile0.png").toURI))
    assert(imageEq(Image(256, 256).overlay(tile0), Image.fromResource("/tile0.png")))

    val tile1tl = tile(temperatures, colors, 1, 0, 0)
//    tile1tl.output(Paths.get(getClass.getResource("/tile1tl.png").toURI))
    assert(imageEq(Image(256, 256).overlay(tile1tl), Image.fromResource("/tile1tl.png")))

    val tile1bl = tile(temperatures, colors, 1, 0, 1)
//    tile1bl.output(Paths.get(getClass.getResource("/tile1bl.png").toURI))
    assert(imageEq(Image(256, 256).overlay(tile1bl), Image.fromResource("/tile1bl.png")))

    val tile1tr = tile(temperatures, colors, 1, 1, 0)
//    tile1tr.output(Paths.get(getClass.getResource("/tile1tr.png").toURI))
    assert(imageEq(Image(256, 256).overlay(tile1tr), Image.fromResource("/tile1tr.png")))

    val tile1br = tile(temperatures, colors, 1, 1, 1)
//    tile1br.output(Paths.get(getClass.getResource("/tile1br.png").toURI))
    assert(imageEq(Image(256, 256).overlay(tile1br), Image.fromResource("/tile1br.png")))
  }

}
