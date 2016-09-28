package platformer

import gamefx._
import javafx.scene.input.KeyCode

object Platformer extends Game {
  
  val width = 24.0
  val height = 13.0
  var scroll = Pt(0, 0)
  var scrollTarget = Pt(0, 0)
  
  var level = Map[(Int, Int), Tile]()
  
  var entities = Vector[Entity](
      new Player(Pt(10, 2))
  )
  
  override def init() = {
    level = PlatformerEditor.loadLevel()
  }
  def step() = {
    for (entity <- entities) {
      entity.step(dt)
    }
  }
  def draw() = {
    buffer.drawPgram(Scale(width, height), Color(0xFF4DA6FF))
    val bufScroll = buffer.transform(Translate(-scroll))
    scroll = Math.pow(0.02, dt) * scroll + (1 - Math.pow(0.02, dt)) * scrollTarget
    for (((x, y), tile) <- level) {
      tile.draw(bufScroll.transform(Translate(x, y)))
    }
    for (entity <- entities) {
      entity.draw(bufScroll.transform(Translate(entity.pos.x, entity.pos.y)))
    }
  }
  
  def tilesAt(pos: Pt, width: Double, height: Double) = {
    // TODO: make faster
    for (
      ((x, y), tile) <- level;
      if !(pos.x >= x + 1 || pos.x + width <= x || pos.y >= y + 1 || pos.y + height <= y)
    ) yield (x, y) -> tile
  }
  
}

trait Entity {
  var pos: Pt
  def step(dt: Double): Unit
  def draw(buffer: Buffer): Unit
  
}

class Player(var pos: Pt) extends PhysicsEntity {
  val width = 0.9
  val height = 1.88
  sealed trait ScrollMode
  case object Leftwards extends ScrollMode
  case object Rightwards extends ScrollMode
  var mode = Rightwards: ScrollMode
  var freeMode = false
  def draw(buffer: Buffer) = {
    buffer.drawPgram(Scale(width, height), Color(0xFFFF0000))
  }
  override def gravity = if (Platformer.down(KeyCode.W)) -16 else -32
  override def step(dt: Double) = {
    super.step(dt)
    if (!floors.isEmpty && Platformer.pressed(KeyCode.W)) {
      vel += Pt(0, 16)
    }
    ceilings.collect { case (pos, Wood) => pos -> Wood }.foreach { case (pos, _) =>
      Platformer.level -= pos
    }
    val runBoost = if (Platformer.down(KeyCode.SHIFT)) 2 else 1
    val airBoost = if (floors.isEmpty) 0.25 else 1
    val moveBoost = runBoost * airBoost
    if (Platformer.down(KeyCode.A)) {
      vel += dt * Pt(moveBoost * -8, 0)
    }
    if (Platformer.down(KeyCode.D)) {
      vel += dt * Pt(moveBoost * 8, 0)
    }
    if (!floors.isEmpty || freeMode) {
      if (!floors.isEmpty) freeMode = false
      Platformer.scrollTarget = Pt(Platformer.scrollTarget.x, pos.y - Platformer.height / 4)
    }
    if (pos.y < Platformer.scroll.y || pos.y + height > Platformer.scroll.y + Platformer.height) {
      freeMode = true
    }
    val myX = pos.x + width/2
    if (myX - Platformer.scroll.x < Platformer.width / 4) {
      mode = Leftwards
    }
    if (myX - Platformer.scroll.x > Platformer.width - Platformer.width / 4) {
      mode = Rightwards
    }
    mode match {
      case Leftwards =>
        val tx = myX - Platformer.width + Platformer.width / 2.5 min Platformer.scrollTarget.x
        Platformer.scrollTarget = Pt(tx, Platformer.scrollTarget.y)
      case Rightwards =>
        val tx = myX - Platformer.width / 2.5 max Platformer.scrollTarget.x
        Platformer.scrollTarget = Pt(tx, Platformer.scrollTarget.y)
    }
  }
}

trait Tile {
  
  def draw(buffer: Buffer)
  
}

trait Solid extends Tile {
  def solidLeft: Boolean
  def solidRight: Boolean
  def solidTop: Boolean
  def solidBottom: Boolean
}

object Brick extends Solid {
  
  def solidLeft = true
  def solidRight = true
  def solidTop = true
  def solidBottom = true
  
  def draw(buffer: Buffer) {
    buffer.drawPgram(Identity, Color(0xFF993300))
  }
  
}
object Wood extends Solid {
  
  def solidLeft = true
  def solidRight = true
  def solidTop = true
  def solidBottom = true
  
  def draw(buffer: Buffer) {
    buffer.drawPgram(Identity, Color(0xFFFF9933))
  }
  
}

object Platform extends Solid {
  
  def solidLeft = false
  def solidRight = false
  def solidTop = true
  def solidBottom = false
  
  def draw(buffer: Buffer) {
    buffer.drawPgram(Identity, Color(0xFF993300))
    buffer.drawPgram(Scale(1, 0.95), Color(0xFFCC4400))
  }
  
}