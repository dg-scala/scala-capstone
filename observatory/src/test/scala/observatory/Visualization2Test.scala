package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {
  import Visualization2._

  test("bilinear interpolation") {
    val d00 = -1.0
    val d01 = 50.0
    val d10 = 49.07195176227695
    val d11 = 0.0
    assert(bilinearInterpolation(0.1, 0.1, d00, d01, d10, d11) == 8.106475658604927)
    assert(bilinearInterpolation(0, 0, d00, d01, d10, d11) == d00)
    assert(bilinearInterpolation(0, 1, d00, d01, d10, d11) == d01)
    assert(bilinearInterpolation(1, 0, d00, d01, d10, d11) == d10)
    assert(bilinearInterpolation(1, 1, d00, d01, d10, d11) == d11)
  }

  test("visualiseGrid should generate correct image") {
    val colors = List(
      (5.0,Color(255,0,0)),
      (30.0,Color(0,0,255))
    )
    val (zoom, x, y) = (0, 0, 0)

  }
}
