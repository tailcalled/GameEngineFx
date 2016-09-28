package movingbox

import gamefx._

object MovingBox extends Game {
  
  val width = 24.0
  val height = 16.0
  
  var t = 0.0
  
  def step() = {
    t += 1/60.0
  }
  def draw() = {
    buffer.drawPgram(Scale(width, height), Color(0xFFFFFFFF))
    for (x <- 0 until 6; y <- 0 until 4) {
      val buf = buffer.transform(Translate(x * 4, y * 4))
      buf.drawPgram(Translate(1.5 + Math.sin(t), 1 + Math.cos(t)) compose RotateAt(-t, 0.5, 0.5), Color(0xFFFF0000))
    }
  }
  
}