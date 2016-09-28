package gamefx

trait Aff2D {
  implicit class GeomDouble(val s: Double) {
    def *(p: Pt) = p * s
  }
  def Translate(tx: Double, ty: Double) = Tx(1, 0, tx, 0, 1, ty)
  def Translate(pt: Pt): Tx = Translate(pt.x, pt.y)
  def Scale(sx: Double, sy: Double) = Tx(sx, 0, 0, 0, sy, 0)
  def Rotate(theta: Double) = Tx(math.cos(theta), -math.sin(theta), 0, math.sin(theta), math.cos(theta), 0)
  def RotateAt(theta: Double, x: Double, y: Double) = Translate(x, y) compose Rotate(theta) compose Translate(-x, -y)
  val Identity = Tx(1, 0, 0, 0, 1, 0)
}
case class Pt(x: Double, y: Double) {
  def +(t: Pt) = Pt(x + t.x, y + t.y)
  def -(t: Pt) = Pt(x - t.x, y - t.y)
  def unary_- = Pt(-x, -y)
  def *(t: Pt) = x * t.x + y * t.y // inner product
  def *(s: Double) = Pt(x * s, y * s)
  def /(s: Double) = Pt(x / s, y / s)
  def **(t: Pt) = x * t.y - t.x * y // cross product
  def abs2 = x * x + y * y
  def abs = math.sqrt(abs2)
  def norm = if (abs > 1e-30) this / abs else Pt(0, 0)
  def *&(t: Pt) = Pt(x * t.x - y * t.y, x * t.y + t.x * y) // complex multiplication
  def cross = Pt(-y, x)
}
case class Tx(
    mxx: Double, mxy: Double, tx: Double,
    myx: Double, myy: Double, ty: Double) {
  def compose(t: Tx) = Tx(
      mxx * t.mxx + mxy * t.myx, mxy * t.myy + mxx * t.mxy, tx + mxx * t.tx + mxy * t.ty,
      myx * t.mxx + myy * t.myx, myy * t.myy + myx * t.mxy, ty + myy * t.ty + myx * t.tx
  )
  def apply(p: Pt) = Pt(mxx * p.x + mxy * p.y + tx, myy * p.y + myx * p.x + ty)
}