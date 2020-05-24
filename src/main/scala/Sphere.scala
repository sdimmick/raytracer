import Vec3.Point3

class Sphere(center: Point3, radius: Double) extends Hittable {
  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val oc = r.origin - center
    val a = r.direction.lengthSquared
    val halfB = Vec3.dot(oc, r.direction)
    val c = oc.lengthSquared - radius * radius
    val discriminant = halfB * halfB - a * c

    if (discriminant > 0) {
      val root = Math.sqrt(discriminant)
      var temp = (-halfB - root) / a
      if (temp < tMax && temp > tMin) {
        return Some(hitRecord(temp, r))
      }
      temp = (-halfB + root) / a
      if (temp < tMax && temp > tMin) {
        return Some(hitRecord(temp, r))
      }
    }

    None
  }

  private def hitRecord(t: Double, r: Ray): HitRecord = {
    val p = r.at(t)
    val outwardNormal = (p - center) / radius
    HitRecord(t, p, r, outwardNormal)
  }
}
