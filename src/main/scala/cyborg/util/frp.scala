package cyborg.util

import scala.concurrent.duration._

import scalaz._, Scalaz._
import scalaz.concurrent.{Promise, Task}
import _root_.io.dylemma.frp._

import cyborg.exceptions.AlreadyShutdown
import cyborg.util.execution._
import cyborg.util.task._
import cyborg.util.scalazext._

object frp {
  implicit class EventStreamCyborgExtras[A](val self: EventStream[A]) extends AnyVal {
    def eagerZip[B](that: EventStream[B])
                   (implicit sec: ScheduledExecutionContext): EventStream[(A, B)] = {
      new EagerlyZippedEventStream[A, B](self, that)
    }

    def ==> [U](f: A => U)(implicit obs: Observer, sec: ScheduledExecutionContext): Unit = {
      self.foreach { a =>
        Task.create(f(a)).runAsync(_.leftMap(_.printStackTrace()))
      }
    }

    def attemptEach[U](f: A => U)(implicit obs: Observer): Unit = {
      self.foreach { e =>
        \/ fromTryCatch {
          f(e)
        } leftMap { t =>
          t.printStackTrace()
        }
      }
    }

    def attemptEachTimed[U](millis: Long)(f: A => U)(implicit obs: Observer, sec: ScheduledExecutionContext): Unit = {
      import scalaz.concurrent.Task
      self.foreach { e =>
        Task.delay(f(e)).timed(millis).attemptRun.fold({ t =>
          t.printStackTrace()
        }, identity)
      }
    }

  }

  implicit class EventSourceCyborgExtras[A](val self: EventSource[A]) extends AnyVal {
    def <== (a: A): Unit = {
      \/.fromTryCatchNonFatal(self.fire(a)).leftMap(t => t.printStackTrace())
    }

    def attemptFire(event: A)(implicit sec: ScheduledExecutionContext): Promise[Unit] = {
      val p = Promise.emptyPromise[Unit]
      Task.create(self.fire(event)).runAsync(_.fold({ t =>
        cyborg.Log.$e(s"FIRE $event failed: " + t.getStackTraceString)
        p.break
      }, _ => p.fulfill(())))
      p
    }

    def attemptFireAndStop(event: A)(implicit sec: ScheduledExecutionContext): Unit = {
      Task.create {
        self.fire(event)
        self.stop
      }.runAsync(_.fold({ t =>
        cyborg.Log.$e(s"FIRE $event failed: " + t.getStackTraceString)
        Option(t)
      }, _ => None))
    }

    def fireIn(d: Duration)(event: A)(implicit sec: ScheduledExecutionContext): Unit = {
      sec.schedule(new Runnable {
        override def run(): Unit = {
          \/.fromTryCatch(self.fire(event)).leftMap { t =>
            cyborg.Log.$e("Delayed FIRE failed: " + t.getStackTraceString)
          }
        }
      }, d.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
    }
  }

  case class SimpleObserver() extends Observer {
    private val references = collection.mutable.ListBuffer[Any]()
    private var _shutdown = false

    override def add(ref: Any): Unit = {
      if (_shutdown) throw AlreadyShutdown("Observer has already been shutdown")
      else references += ref
    }

    def shutdown(): Unit = {
      _shutdown = true
      references.clear()
    }
  }

  class EventVar[A](initial: A)(implicit obs: Observer) extends EventSource[A] {
    private var _value = initial
    def value: A = _value

    override def fire(event: A): Unit = {
      debug.warnOnUiThread // TODO: remove this line
      if (stopped) throw new IllegalStateException("Cannot fire events from a stopped EventSource")
      else {
        _value = event
        produce(Fire(event))
      }
    }

    def attemptFire(event: A)(implicit sec: ScheduledExecutionContext): Promise[Unit] = {
      val p = Promise.emptyPromise[Unit]
      Task.create(fire(event)).runAsync(_.fold({ t =>
        cyborg.Log.$e(s"FIRE $event failed: " + t.getStackTraceString)
        p.break
      }, _ => p.fulfill(())))
      p
    }
  }

  abstract class EventPipeVar[A, B](initial: A, parent: EventStream[B])
                                   (implicit obs: Observer,
                                    sec: ScheduledExecutionContext) extends EventSource[A] {
    private var _value = initial
    def value: A = _value
    def handle(event: B): A
    parent attemptEach { event: B =>
      if (!stopped) _value = handle(event)
      this.attemptFire(_value)
    }
  }

  implicit def value[A](ev: EventVar[A]): A = ev.value
  implicit def value[A](ev: EventPipeVar[A, _]): A = ev.value

  def eventVar[A](initial: A)(implicit obs: Observer): EventVar[A] = {
    assert(obs != null)
    new EventVar[A](initial)
  }

  def eventPipeVar[A, B](initial: A, parent: EventStream[B])(handler: B => A)
                        (implicit obs: Observer, sec: ScheduledExecutionContext): EventPipeVar[A, B] = {
    new EventPipeVar[A, B](initial, parent) {
      override def handle(event: B): A = handler(event)
    }
  }

  def eventOption[A](parent: EventStream[A])
                    (implicit obs: Observer, sec: ScheduledExecutionContext): EventPipeVar[Option[A], A] = {
    new EventPipeVar[Option[A], A](None, parent) {
      override def handle(event: A): Option[A] = Some(event)
    }
  }

  class EventDebouncePipe[A](val parent: EventStream[A], val delay: Duration)
                            (implicit val sec: ScheduledExecutionContext, obs: Observer) extends EventSource[A] {
    private var newest: Option[A] = None
    private def handle(event: A): Unit = {
      synchronized {
        newest.cata({ _ =>
          newest = Some(event)
          // Task should already have been scheduled
        }, {
          newest = Some(event)
          Task.create {
            newest.foreach(x => this <== x)
            newest = None
          } .after(delay).runAsync(_.logLeft)
        })
      }
    }
    parent.attemptEach { event: A =>
      if (!stopped) handle(event)
    }
  }

  def debounced[A](parent: EventStream[A], delay: Duration)(implicit sec: ScheduledExecutionContext, obs: Observer) =
    new EventDebouncePipe[A](parent, delay)

  // EventStream that will automatically fire events transform(from) to transform(to) over the given duration,
  // and with specified delay in between.
  class EventAnimator[A] (from: Float, to: Float, duration: Duration, delay: Duration)
                         (val transform: Float => A)
                         (implicit val sec: ScheduledExecutionContext) extends EventSource[A] {
    import scalaz.concurrent._
    import cyborg.util.task._

    private val (a, b) = if (from < to) (from, to) else (to, from)
    private val mag = b - a
    private val d = duration.toMillis
    private var started = 0l

    def goAfter(after: Duration): Promise[Unit] = {
      val promise = Promise.emptyPromise[Unit]
      started = systemTime
      var task: Task[Unit] = Task.delay {}
      task = Task.create {
        val t = systemTime - started
        if (t >= d) {
          this.attemptFireAndStop(transform(b))
          promise fulfill Unit
        }
        else {
          val x = a + (t.toFloat / d.toFloat) * mag
          this.attemptFire(transform(x))
          task.runAfterDelay(delay)(_ => Unit)
        }
      }
      task.runAfterDelay(after)(_ => Unit)
      promise
    }

    def go = goAfter(0.millis)

    /*
    def go: Unit = {
      started = systemTime
      Task.delay {
        var t = systemTime - started
        while(t < d) {
          val x = a + (t.toFloat / d.toFloat) * mag
          fire(transform(x))
          Thread.sleep(milliseconds(delay))
          t = systemTime - started
        }
        fire(transform(b))
        stop
      } .runAsync(_ => Unit)
    }*/
  }

  object EventAnimator {
    def apply[A](from: Float, to: Float, duration: Duration, delay: Duration)
                (transform: Float => A)
                (implicit sec: ScheduledExecutionContext) =
      new EventAnimator[A](from, to, duration, delay)(transform)(sec)

    // Sigmoid function to use with animation, works in range (0:1)
    def sigmoid(x: Float): Float = x match {
      case 0f => 0f
      case 1f => 1f
      case y => (math.tanh(y * 2.5).toFloat + 1f) / 2f
    }
  }

  // When either of the parent streams fire, a new event will fire with the last value for the other stream if available
  private class EagerlyZippedEventStream[A, B](val leftParent: EventStream[A], val rightParent: EventStream[B])
                                              (implicit sec: ScheduledExecutionContext)
    extends EventJoin[A, B, (A, B)] {

    private var lastLeft: Option[A] = None
    private var lastRight: Option[B] = None

    override protected def handle(event: Either[Event[A], Event[B]]): Boolean = {
      event match {
        case Left(Stop) | Right(Stop) =>
          stop
          false
        case Left(Fire(left)) =>
          lastLeft = Option(left)
          lastRight foreach (x => this.attemptFire((left, x)))
          true
        case Right(Fire(right)) =>
          lastRight = Option(right)
          lastLeft foreach (x => this.attemptFire((x, right)))
          true
      }
    }
  }
}
