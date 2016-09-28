package platformer

import gamefx._
import javafx.scene.input.KeyCode
import java.util.Scanner
import java.io.File
import java.io.PrintStream

object PlatformerEditor extends Game {
  
  val width = Platformer.width
  val height = Platformer.height
  var scroll = Pt(0, 0)
  val palette = Vector[Tile](Brick, Platform, Wood)
  var tiles = palette
  
  var level = Map[(Int, Int), Tile]()
  
  var mx = 0
  var my = 0
  
  override def init() = {
    level = loadLevel()
    val saveThread = new Thread() {
      override def run() = {
        while (!stopped) {
          val out = new PrintStream(new File("level.txt"))
          out.println(level.size)
          for (((x, y), tile) <- level) {
            out.println(s"$x $y ${palette.indexOf(tile)}")
          }
        }
      }
    }
    saveThread.start()
  }
  def loadLevel() = {
    val file = new File("level.txt")
    var l = Map[(Int, Int), Tile]()
    if (file.exists()) {
      val in = new Scanner(new File("level.txt"))
      for (i <- 0 until in.nextInt()) {
        l += (in.nextInt(), in.nextInt()) -> palette(in.nextInt())
      }
      in.close()
    }
    l
  }
  def step() = {
    if (down(KeyCode.LEFT)) scroll += dt * Pt(-4, 0)
    if (down(KeyCode.RIGHT)) scroll += dt * Pt(4, 0)
    if (down(KeyCode.DOWN)) scroll += dt * Pt(0, -4)
    if (down(KeyCode.UP)) scroll += dt * Pt(0, 4)
    if (pressed(KeyCode.A)) {
      tiles = tiles.tail :+ tiles.head
    }
    if (pressed(KeyCode.D)) {
      tiles = tiles.last +: tiles.init
    }
    mx = (mouse.x + scroll.x).floor.toInt
    my = (mouse.y + scroll.y).floor.toInt
    if (down(KeyCode.X)) {
      level -= ((mx, my))
    }
    if (mdown) {
      level += (mx, my) -> tiles.head
    }
  }
  def draw() = {
    buffer.drawPgram(Scale(width, height), Color(0xFF4DA6FF))
    val bufScroll = buffer.transform(Translate(-scroll))
    bufScroll.drawPgram(Scale(0.1, 0.1) compose Translate(-0.5, -0.5), Color(0xFF000000))
    val ox = 2 * (scroll.x/2 - (scroll.x/2).floor)
    val oy = 2 * (scroll.y/2 - (scroll.y/2).floor)
    for (x <- 0 to width.ceil.toInt + 1; y <- 0 to height.ceil.toInt + 1) {
      if ((x + y) % 2 == 0) {
        buffer.drawPgram(Translate(x - ox, y - oy), Color(0x10FFFFFF))
      }
    }
    for (((x, y), tile) <- level) {
      tile.draw(bufScroll.transform(Translate(x, y)))
    }
    bufScroll.drawPgram(Translate(mx, my), Color(0x10000000))
    buffer.drawPgram(Translate(width - 2.5, height - 2.5) compose Scale(2, 2), Color(0xFF7F7F7F))
    tiles.head.draw(buffer.transform(Translate(width - 2, height - 2)))
  }
  
}