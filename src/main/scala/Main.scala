object Main {
  def printImage(width: Int, height: Int): Unit = {
    // P3 image header
    println(s"P3\n$width $height\n255")

    for (j <- height - 1 to 0 by -1) {
      for (i <- 0 until width) {
        val r = i.asInstanceOf[Double] / (width - 1)
        val g = j.asInstanceOf[Double] / (height - 1)
        val b = 0.25
        val pixelColor = new Vec3(r, g, b)
        pixelColor.writeColor()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    printImage(256, 256)
  }
}
