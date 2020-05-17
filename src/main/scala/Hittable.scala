import Vec3.Point3

case class HitRecord(
  p: Point3,
  normal: Vec3,
  t: Double
)

trait Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double, rec: HitRecord): Option[HitRecord]
}
