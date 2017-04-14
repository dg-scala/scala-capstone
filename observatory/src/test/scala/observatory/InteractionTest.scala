package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  import Interaction._
  import Visualization._

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

  test("tileLocation for various zooms") {
    val l000 = tileLocation(0, 0, 0)
    val l011 = tileLocation(0, 1, 1)
    assert(l000 == Location(85.05112877980659,-180.0))
    assert(l011 == Location(-85.05112877980659,180.0))

    val l100 = tileLocation(1, 0, 0)
    val l101 = tileLocation(1, 0, 1)
    val l110 = tileLocation(1, 1, 0)
    val l111 = tileLocation(1, 1, 1)
    val l122 = tileLocation(1, 2, 2)
    assert(l100 == Location(85.05112877980659,-180.0))
    assert(l101 == Location(0.0,-180.0))
    assert(l110 == Location(85.05112877980659,0.0))
    assert(l111 == Location(0.0,0.0))
    assert(l122 == Location(-85.05112877980659,180.0))
  }

  test("tile on zoom 1") {
    val tile000 = tile(temperatures, colors, 0, 0, 0)
    tile000.output(homeFile("Desktop/tile000.png"))

    val tile100 = tile(temperatures, colors, 1, 0, 0)
    tile100.output(homeFile("Desktop/tile100.png"))

    val tile101 = tile(temperatures, colors, 1, 0, 1)
    tile101.output(homeFile("Desktop/tile101.png"))

    val tile110 = tile(temperatures, colors, 1, 1, 0)
    tile110.output(homeFile("Desktop/tile110.png"))

    val tile111 = tile(temperatures, colors, 1, 1, 1)
    tile111.output(homeFile("Desktop/tile111.png"))

  }

}
