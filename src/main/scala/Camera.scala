import Vec3.Point3

object Camera {
  private val aspectRatio = 16.0 / 9.0
  private val viewportHeight = 2.0
  private val viewportWidth = aspectRatio * viewportHeight
  private val focalLength = 1.0

  private val origin = new Point3(0, 0, 0)
  private val horizontal = new Vec3(viewportWidth, 0, 0)
  private val vertical = new Vec3(0, viewportHeight, 0)
  private val lowerLeftCorner = origin - horizontal / 2 -
      vertical / 2 - new Vec3(0, 0, focalLength)

  def getRay(u: Double, v: Double): Ray = {
    new Ray(origin, lowerLeftCorner + horizontal * u + vertical * v - origin)
  }

}