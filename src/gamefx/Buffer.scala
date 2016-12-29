package gamefx

class Image private[gamefx](private[gamefx] val img: javafx.scene.image.Image)
case class Color(argb: Int) extends AnyVal {
  def a = (argb >> 24) & 0xFF
  def r = (argb >> 16) & 0xFF
  def g = (argb >>  8) & 0xFF
  def b = (argb >>  0) & 0xFF
}

trait Buffer { self =>
  
  def drawImage(tx: Tx, img: Image): Unit
  def drawPgram(tx: Tx, color: Color): Unit // apply tx to a 1x1 square and fill it with color
  def drawPoly(tx: Tx, pts: Seq[Pt], color: Color): Unit
  def transform(t: Tx): Buffer = new Buffer {
    def drawImage(tx: Tx, img: Image) = self.drawImage(t compose tx, img)
    def drawPgram(tx: Tx, color: Color) = self.drawPgram(t compose tx, color)
    def drawPoly(tx: Tx, pts: Seq[Pt], color: Color) = self.drawPoly(t compose tx, pts, color)
    override def transform(t2: Tx): Buffer = self.transform(t compose t2)
  }
  
}

private [gamefx] class ContextBuffer(gfx: javafx.scene.canvas.GraphicsContext) extends Buffer {
  
  def tx2Tform(tx: Tx) = new javafx.scene.transform.Affine(
      tx.mxx, tx.mxy, tx.tx,
      tx.myx, tx.myy, tx.ty)
  def convertColor(color: Color) = javafx.scene.paint.Color.rgb(color.r, color.g, color.b, color.a / 255.0)
  
  def drawImage(tx: Tx, img: Image) = {
    gfx.setTransform(tx2Tform(tx compose Scale(1.01, -1.01)))
    gfx.drawImage(img.img, 0, img.img.getHeight)
  }
  def drawPgram(tx: Tx, color: Color) = {
    gfx.setTransform(tx2Tform(tx))
    gfx.setFill(convertColor(color))
    gfx.fillRect(0, 0, 1.01, 1.01)
  }
  def drawPoly(tx: Tx, pts: Seq[Pt], color: Color) = {
    gfx.setTransform(tx2Tform(tx))
    gfx.setFill(convertColor(color))
    gfx.fillPolygon(pts.view.map(_.x).toArray, pts.view.map(_.y).toArray, pts.length)
  }
  
}