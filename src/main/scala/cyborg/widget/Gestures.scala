package cyborg.widget

import android.view.{MotionEvent, GestureDetector}
import cyborg.util.execution.ScheduledExecutionContext
import io.dylemma.frp._
import cyborg.util.frp._

object Gestures {
  trait Gesture

  trait Point {
    def x: Float
    def y: Float
  }

  trait Vector {
    def start: Point
    def end: Point
  }

  trait Velocity {
    def xv: Float
    def yv: Float
  }

  case class OnlyPoint(x: Float, y: Float) extends Point

  case class UpGesture(x: Float, y: Float) extends Gesture with Point
  case class DownGesture(x: Float, y: Float) extends Gesture with Point
  case class FlingGesture(start: Point, end: Point, xv: Float, yv: Float)
    extends Gesture with Vector with Velocity
  case class LongPressGesture(x: Float, y: Float) extends Gesture with Point
  case class ScrollGesture(start: Point, end: Point) extends Gesture with Vector
  case class ShowPressGesture(x: Float, y: Float) extends Gesture with Point
  case class SingleTapGesture(x: Float, y: Float) extends Gesture with Point

  private class GestureListener(source: EventSource[Gesture])
                               (implicit val sec: ScheduledExecutionContext) extends GestureDetector.OnGestureListener {
    override def onDown(e: MotionEvent): Boolean = {
      source attemptFire DownGesture(e.getX, e.getY)
      true
    }
    override def onFling(e1: MotionEvent, e2: MotionEvent, xv: Float, yv: Float): Boolean = {
      source attemptFire FlingGesture(OnlyPoint(e1.getX, e1.getY), OnlyPoint(e2.getX, e2.getY), xv, yv)
      true
    }
    override def onLongPress(e: MotionEvent): Unit = {
      source attemptFire LongPressGesture(e.getX, e.getY)
      true
    }
    override def onScroll(e1: MotionEvent, e2: MotionEvent, dx: Float, dy: Float): Boolean = {
      source attemptFire ScrollGesture(OnlyPoint(e1.getX, e1.getY), OnlyPoint(e2.getX, e2.getY))
      true
    }
    override def onShowPress(e: MotionEvent): Unit = {
      source attemptFire ShowPressGesture(e.getX, e.getY)
    }
    override def onSingleTapUp(e: MotionEvent): Boolean = {
      source attemptFire SingleTapGesture(e.getX, e.getY)
      true
    }
  }

  case class GestureEventSource()(implicit val sec: ScheduledExecutionContext) extends EventSource[Gesture] {
    private val detector = new GestureDetector(new GestureListener(this))
    def handle(e: MotionEvent): Boolean = {
      if (e.getAction == MotionEvent.ACTION_UP) this.attemptFire(UpGesture(e.getX, e.getY))
      detector.onTouchEvent(e)
    }
  }

}
