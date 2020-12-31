import Vec3.{Color, Point3}

object Main {

  def rayColor(r: Ray, world: Seq[Hittable], depth: Int): Color = {
    if (depth <= 0) {
      return new Color(0, 0, 0)
    }

    world.hit(r, 0.001, Double.PositiveInfinity) match {
      case Some(hit) => {
        hit.mat.scatter(r, hit) match {
          case sr: ScatterRecord if sr.scattered => {
            Vec3.*(sr.attenuation, (rayColor(sr.ray, world, depth - 1)))
          }
          case _ => new Color(0, 0, 0)
        }

//        val target = hit.p + Vec3.randomInHemisphere(hit.normal)
//        rayColor(new Ray(hit.p, target - hit.p), world, depth - 1) * 0.5
      }
      case _         => {
        val unitDirection = Vec3.unit(r.direction)
        val t = (unitDirection.y + 1.0) * 0.5
        new Color(1.0, 1.0, 1.0) * (1.0 - t) +
          new Color(0.5, 0.7, 1.0) * t
      }
    }
  }

  def hitSphere(center: Point3, radius: Double, r: Ray): Double = {
    val oc = r.origin - center
    val a = r.direction.lengthSquared
    val halfB = Vec3.dot(oc, r.direction)
    val c = oc.lengthSquared - radius * radius
    val discriminant = halfB * halfB - a * c

    if (discriminant < 0) -1.0 else (-halfB - Math.sqrt(discriminant)) / a
  }

  def printImage(width: Int, height: Int, samplesPerPixel: Int, maxDepth: Int): Unit = {
    // P3 image header
    println(s"P3\n$width $height\n255")

    val materialGround = new Lambertian(new Color(0.8, 0.8, 0.0))
    val materialCenter = new Lambertian(new Color(0.7, 0.3, 0.3))

    val world = Seq(
      new Sphere(new Point3(0, 0, -1), 0.5, materialCenter),
      new Sphere(new Point3(0, -100.5, -1), 100, materialGround)
    )

    for (j <- height - 1 to 0 by -1) {
      System.err.print(s"\rScanlines remaining: $j")
      System.err.flush()
      for (i <- 0 until width) {
        val pixelColor = (0 until samplesPerPixel).foldLeft(new Color(0, 0, 0)) { (color, s) =>
          val u = (i + Math.random()) / (width - 1)
          val v = (j + Math.random()) / (height - 1)
          val r = Camera.getRay(u, v)
          color + rayColor(r, world, maxDepth)
        }

        pixelColor.writeColor(samplesPerPixel)
      }
    }
    System.err.print("\nDone\n")
  }

  def main(args: Array[String]): Unit = {
    val aspectRatio = 16.0 / 9.0
    val width = 100
    val height = (width / aspectRatio).asInstanceOf[Int]
    val samplesPerPixel = 60
    val maxDepth = 50

    printImage(width, height, samplesPerPixel, maxDepth)
  }
}
