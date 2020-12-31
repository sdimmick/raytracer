import Vec3.Color

case class ScatterRecord(
  ray: Ray,
  scattered: Boolean,
  attenuation: Color
)

abstract class Material {
  def scatter(ray: Ray, rec: HitRecord): ScatterRecord
}

class Lambertian(albedo: Color) extends Material {
  override def scatter(ray: Ray, rec: HitRecord): ScatterRecord = {
    var scatterDirection = rec.normal + Vec3.randomUnitVector()
    // Catch degenerate scatter direction
    if (scatterDirection.nearZero) {
      scatterDirection = rec.normal
    }
    ScatterRecord(
      new Ray(rec.p, scatterDirection),
      scattered = true,
      albedo
    )
  }
}
