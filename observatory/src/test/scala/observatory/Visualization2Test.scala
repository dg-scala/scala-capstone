package observatory

import java.nio.file.Paths

import com.sksamuel.scrimage.Image
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {
  import Visualization2._

  def imageEq(im1: Image, im2: Image): Boolean = im1.pixels.sameElements(im2.pixels)

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
    val grid: (Int, Int) => Double = (_, _) => 12.5
    val img = visualizeGrid(grid, colors, zoom, x, y)
//    img.output(Paths.get(getClass.getResource("/gridTile.png").toURI))
    assert(imageEq(Image(256, 256).overlay(img), Image.fromResource("/gridTile.png")))
  }
}
