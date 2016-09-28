package platformer

import gamefx._

trait PhysicsEntity extends Entity {
  val width: Double
  val height: Double
  var vel = Pt(0, 0)
  def gravity = -32
  var colVert = Map[(Int, Int), Tile]()
  var colSide = Map[(Int, Int), Tile]()
  var walls = Map[(Int, Int), Solid]()
  var floors = Map[(Int, Int), Solid]()
  var ceilings = Map[(Int, Int), Solid]()
  def step(dt: Double) = {
    vel += dt * Pt(0, gravity)
    vel *= Math.exp(-dt * 0.5)
    colSide =
      Platformer.tilesAt(pos + dt * Pt(vel.x, 0), width, height) --
      Platformer.tilesAt(pos, width, height).keys
    walls = colSide.collect {
      case (pos, t: Solid) if t.solidLeft && vel.x > 0 || t.solidRight && vel.x < 0 => pos -> t
    }
    if (walls.isEmpty) {
      pos += dt * Pt(vel.x, 0)
    }
    else {
      val x =
        if (vel.x > 0) {
          walls.keys.map(_._1).min - width
        }
        else if (vel.x < 0) {
          walls.keys.map(_._1).max + 1
        }
        else pos.x
      vel = Pt(0, vel.y)
      pos = Pt(x, pos.y)
    }
    colVert =
      Platformer.tilesAt(pos + dt * Pt(0, vel.y), width, height) --
      Platformer.tilesAt(pos, width, height).keys
    floors = colVert.collect {
      case (pos, t: Solid) if t.solidTop && vel.y < 0 => pos -> t
    }
    ceilings = colVert.collect {
      case (pos, t: Solid) if t.solidBottom && vel.y > 0 => pos -> t
    }
    if ((floors ++ ceilings).isEmpty) {
      pos += dt * Pt(0, vel.y)
    }
    else {
      val y =
        if (vel.y > 0) {
          ceilings.keys.map(_._2).min - height
        }
        else if (vel.y < 0) {
          floors.keys.map(_._2).max + 1
        }
        else pos.y
      vel = Pt(vel.x, 0)
      pos = Pt(pos.x, y)
    }
    if (!floors.isEmpty) {
      vel = Pt(vel.x * Math.exp(-dt * 4), vel.y)
    }
  }
}