class Vec3(e0: Double, e1: Double, e2: Double) {
  private val e = Array(e0, e1, e2)

  def x: Double = e(0)

  def y: Double = e(1)

  def z: Double = e(2)

  def - = Vec3(e.map(el => -el))

  def apply(i: Int): Double = e(i)

  def +(v: Vec3) = Vec3(e.zip(v.e).map { case (thisE, thatE) => thisE + thatE })

  def *(t: Double) = Vec3(e.map(_ * t))

  def /(t: Double): Vec3 = this * (1/t)

  def length: Double = Math.sqrt(lengthSquared)

  def lengthSquared: Double = e.map(el => el * el).sum

  override def toString: String = e.mkString(" ")

  def writeColor(): Unit = {
    println(e.map(el => (el * 255.999).asInstanceOf[Int]).mkString(" "))
  }
}

object Vec3 {
  def apply() = new Vec3(0, 0, 0)

  def apply(e: Seq[Double]): Vec3 = {
    e match {
      case Seq(e0, e1, e2) => new Vec3(e0, e1, e2)
    }
  }

  def +(u: Vec3, v: Vec3) = Vec3(u.e.zip(v.e).map { case (a, b) => a + b })

  def -(u: Vec3, v: Vec3) = Vec3(u.e.zip(v.e).map { case (a, b) => a - b })

  def *(u: Vec3, v: Vec3) = Vec3(u.e.zip(v.e).map { case (a, b) => a * b })

  def *(t: Double, v: Vec3) = Vec3(v.e.map(_ * t))

  def /(t: Double, v: Vec3) = Vec3(v.e.map(_ / t))

  def dot(u: Vec3, v: Vec3): Double = u.e.zip(v.e).map { case (a, b) => a * b }.sum

  def cross(u: Vec3, v: Vec3): Vec3 = {
    new Vec3(
      u.e(1) * v.e(2) - u.e(2) * v.e(1),
      u.e(2) * v.e(0) - u.e(0) * v.e(2),
      u.e(0) * v.e(1) - u.e(1) * v.e(0)
    )
  }

  def unit(v: Vec3): Vec3 = v / v.length
}