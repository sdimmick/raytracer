import Vec3.{Color, Point3}

object Main {

  def rayColor(r: Ray, world: Seq[Hittable]): Color = {
    world.hit(r, 0, Double.PositiveInfinity) match {
      case Some(hit) => (hit.normal + new Color(1, 1, 1)) * 0.5
      case _         => {
        val unitDirection = Vec3.unit(r.direction)
        val t = (unitDirection.y + 1.0) * 0.5
        new Color(1.0, 1.0, 1.0) * (1.0 - t) +
          new Color(0.5, 0.7, 1.0) * t
      }
    }
  }


  def rayColorOld(r: Ray): Color = {
    var t = hitSphere(new Point3(0, 0, -1), 0.5, r)
    if (t > 0) {
      val n = Vec3.unit(r.at(t) - new Vec3(0, 0, -1))
      return new Color(n.x + 1, n.y + 1, n.z + 1) * 0.5
    }

    val unitDirection = Vec3.unit(r.direction)
    t = (unitDirection.y + 1.0) * 0.5

    new Color(1.0, 1.0, 1.0) * (1.0 - t) +
      new Color(0.5, 0.7, 1.0) * t
  }

  def hitSphere(center: Point3, radius: Double, r: Ray): Double = {
    val oc = r.origin - center
    val a = r.direction.lengthSquared
    val halfB = Vec3.dot(oc, r.direction)
    val c = oc.lengthSquared - radius * radius
    val discriminant = halfB * halfB - a * c

    if (discriminant < 0) -1.0 else (-halfB - Math.sqrt(discriminant)) / a
  }

  def printImage(width: Int, height: Int): Unit = {
    // P3 image header
    println(s"P3\n$width $height\n255")

    val origin = new Point3(0.0, 0.0, 0.0)
    val horizontal = new Vec3(4.0, 0.0, 0.0)
    val vertical = new Vec3(0.0, 2.25, 0.0)
    val lowerLeftCorner = origin - horizontal / 2 - vertical / 2 - new Vec3(0,0,1)

    val world = Seq(
      new Sphere(new Point3(0, 0, -1), 0.5),
      new Sphere(new Point3(0, -100.5, -1), 100)
    )

    for (j <- height - 1 to 0 by -1) {
      for (i <- 0 until width) {
        val u = i.asInstanceOf[Double] / (width - 1)
        val v = j.asInstanceOf[Double] / (height - 1)
        val r = new Ray(origin, lowerLeftCorner + (horizontal * u) + (vertical * v))

        val pixelColor = rayColor(r, world)
        pixelColor.writeColor()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val aspectRatio = 16.0 / 9.0
    val width = 384
    val height = (width / aspectRatio).asInstanceOf[Int]

    printImage(width, height)
  }
}
