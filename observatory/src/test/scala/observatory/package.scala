import com.sksamuel.scrimage.Image

package object observatory {

  def imageEq(im1: Image, im2: Image): Boolean = im1.pixels.sameElements(im2.pixels)

}
