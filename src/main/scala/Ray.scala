import Vec3.Point3

class Ray(val origin: Point3, val direction: Vec3) {

  def at(t: Double): Point3 = origin + (direction * t)

}
