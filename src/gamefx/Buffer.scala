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
  def transform(t: Tx) = new Buffer {
    def drawImage(tx: Tx, img: Image) = self.drawImage(t compose tx, img)
    def drawPgram(tx: Tx, color: Color) = self.drawPgram(t compose tx, color)
  }
  
}

private [gamefx] class ContextBuffer(gfx: javafx.scene.canvas.GraphicsContext) extends Buffer {
  
  def tx2Tform(tx: Tx) = new javafx.scene.transform.Affine(
      tx.mxx, tx.mxy, tx.tx,
      tx.myx, tx.myy, tx.ty)
  
  def drawImage(tx: Tx, img: Image) = {
    gfx.setTransform(tx2Tform(tx compose Scale(1.01, -1.01)))
    gfx.drawImage(img.img, 0, img.img.getHeight)
  }
  def drawPgram(tx: Tx, color: Color) = {
    gfx.setTransform(tx2Tform(tx))
    gfx.setFill(javafx.scene.paint.Color.rgb(color.r, color.g, color.b, color.a / 255.0))
    gfx.fillRect(0, 0, 1.01, 1.01)
  }
  
}