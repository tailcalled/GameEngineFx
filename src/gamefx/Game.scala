package gamefx

import javafx.scene.input.KeyCode

trait Game {
  
  val width: Double
  val height: Double
  private [gamefx] var _stopped = false
  def stopped = _stopped
  private [gamefx] var _dt = 0.0
  def dt = _dt
  private [gamefx] var _buffer: Buffer = null
  def buffer = _buffer
  private [gamefx] var _down = Set[KeyCode]()
  def down = _down
  private [gamefx] var _pressed = Set[KeyCode]()
  def pressed = _pressed
  private [gamefx] var _released = Set[KeyCode]()
  def released = _released
  private [gamefx] var _mouse = Pt(0, 0)
  def mouse = _mouse
  private [gamefx] var _mdown = false
  def mdown = _mdown
  
  final def stop() = { _stopped = true }
  
  def init() = {}
  def step(): Unit
  def draw(): Unit
  
  def main(args: Array[String]) = {
    init()
    run(this)
  }
  
}