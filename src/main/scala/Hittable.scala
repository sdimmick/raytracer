import Vec3.Point3

case class HitRecord(
  p: Point3,
  normal: Vec3,
  mat: Material,
  t: Double,
  frontFace: Boolean
)

object HitRecord {
  def apply(t: Double, p: Point3, r: Ray, outwardNormal: Vec3, mat: Material): HitRecord = {
    val frontFace = Vec3.dot(r.direction, outwardNormal) < 0
    val normal = if (frontFace) outwardNormal else (outwardNormal-)
    new HitRecord(p, normal, mat, t, frontFace)
  }
}

trait Hittable {
  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]
}

object Hittable {
  implicit class RichSeqHittables(hittables: Seq[Hittable]) {
    def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
      var hitAnything = false
      var closestSoFar = tMax
      var closestHit: Option[HitRecord] = None

      hittables.foreach(h => {
        h.hit(r, tMin, closestSoFar).foreach(hitRecord => {
          hitAnything = true
          closestSoFar = hitRecord.t
          closestHit = Some(hitRecord)
        })
      })

      closestHit
    }
  }
}

