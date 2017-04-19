
package object observatory {

  def homeFile(relativePath: String): java.io.File = new java.io.File(s"${sys.env("HOME")}/$relativePath")

  def relativeFile(relativePath: String): java.io.File = {
    val parts = relativePath.split("/")
    val folderPath = parts.take(parts.size - 1).fold("")((f1, f2) =>
      if (f1 == "") f2
      else f1 + "/" + f2)
    val folder = new java.io.File(folderPath)
    folder.mkdirs

    new java.io.File(relativePath)
  }
}
