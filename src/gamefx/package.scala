import javafx.application.Platform
import javafx.animation.AnimationTimer
import javax.swing.SwingUtilities
import javafx.embed.swing.JFXPanel
import javafx.stage.Stage
import javafx.stage.WindowEvent
import javafx.scene.Scene
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent
import javafx.scene.input.KeyCode
import javafx.scene.layout.BorderPane
import javafx.scene.canvas.Canvas
import javafx.event.Event
import javafx.event.EventHandler

package object gamefx extends Aff2D {
  
  private[gamefx] implicit def mkRunnable[A](closure: () => A): Runnable = new Runnable {
    def run() = { closure() }
  }
  private[gamefx] implicit def mkEventHandler[Ev <: Event, A](closure: Ev => A): EventHandler[Ev] = new EventHandler[Ev] {
    def handle(ev: Ev) = { closure(ev) }
  }
  
  def run(game: Game) = {
    SwingUtilities.invokeAndWait(() => {
      val panel = new JFXPanel()
    })
    Platform.runLater(() => {
      val stage = new Stage()
      val canvas = new Canvas()
      val g = canvas.getGraphicsContext2D
      val root = new BorderPane()
      val scene = new Scene(root)
      root.setCenter(canvas)
      var t0 = System.nanoTime()
      var down = Set[KeyCode]()
      var mouse = Pt(0, 0)
      var mdown = false
      scene.setOnKeyPressed((ev: KeyEvent) => {
        down += ev.getCode
      })
      scene.setOnKeyReleased((ev: KeyEvent) => {
        down -= ev.getCode
      })
      scene.setOnMouseMoved((ev: MouseEvent) => {
        mouse = Pt(ev.getX, ev.getY)
      })
      scene.setOnMouseDragged(scene.getOnMouseMoved)
      scene.setOnMousePressed((ev: MouseEvent) => {
        mdown = true
      })
      scene.setOnMouseReleased((ev: MouseEvent) => {
        mdown = false
      })
      stage.setOnCloseRequest((ev: WindowEvent) => {
        game._stopped = true
      })
      lazy val timer: AnimationTimer = new AnimationTimer() {
        def handle(time: Long) = {
          canvas.setWidth(scene.getWidth)
          canvas.setHeight(scene.getHeight)
          g.save()
          val scale =
            if (canvas.getWidth / game.width < canvas.getHeight / game.height)
              canvas.getWidth / game.width
            else
              canvas.getHeight / game.height
          val offx = (canvas.getWidth - game.width * scale) / 2
          val offy = (canvas.getHeight - game.height * scale) / 2
          val t1 = System.nanoTime()
          val dt = (t1 - t0) / 1000000000.0
          t0 = t1
          game._dt = dt
          game._buffer = new ContextBuffer(g).transform(
              Translate(offx, offy) compose
              Scale(scale, scale) compose
              Translate(0, game.height) compose
              Scale(1, -1)
          )
          game._pressed = down -- game._down
          game._released = game._down -- down
          game._down = down
          game._mouse = {
              val m = (mouse - Pt(offx, offy)) / scale
              Pt(m.x, game.height - m.y)
            }
          game._mdown = mdown
          game.step()
          game.draw()
          g.restore()
          g.setFill(javafx.scene.paint.Color.BLACK)
          g.fillRect(0, 0, offx, canvas.getHeight)
          g.fillRect(canvas.getWidth - offx, 0, offx, canvas.getHeight)
          g.fillRect(0, 0, canvas.getWidth, offy)
          g.fillRect(0, canvas.getHeight - offy, canvas.getWidth, offy)
          if (game.stopped) timer.stop()
        }
      }
      stage.setScene(scene)
      timer.start()
      stage.setFullScreen(true)
      stage.show()
    })
  }
  
}