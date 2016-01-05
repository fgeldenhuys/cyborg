package cyborg.droid

import android.content.Context
import android.os.SystemClock
import android.webkit
import android.webkit.WebView

import io.dylemma.frp._

import cyborg.Log._
import cyborg.net.Uri2._
import cyborg.util.debug.warnIfNotOnUiThread
import cyborg.util.execution._
import cyborg.util.string._
import cyborg.util.frp._
import cyborg.util.task._

import scalaz._, scalaz.concurrent._, Scalaz._

class CyborgWebView(
    val context: Context,
    var interceptRequest: Option[String => Option[webkit.WebResourceResponse]] = None
  )(implicit
    sec: ScheduledExecutionContext,
    obs: Observer,
    uit: UiThread
  ) extends WebView(context) { self =>
  private var loadUriPromise: Option[Promise[java.net.URI]] = None

  def scale = eventVar[Float](Task.uiThread(getScale).attemptRun.getOrElse(1.0f))

  def shouldOverrideUrlLoading(url: String): Boolean = false

  val client = new android.webkit.WebViewClient {
    private var previousUrl = "" //for debouncing

    override def onPageFinished(view: WebView, url: String): Unit = {
      super.onPageFinished(view, url)
      $d(s"PAGE finished current=$url previous=$previousUrl")
      previousUrl = url
      loadUriPromise.foreach { r =>
        r.fulfill(url.toUri)
      }
      loadUriPromise = None
    }

    override def shouldInterceptRequest(view: webkit.WebView, url: String): webkit.WebResourceResponse = {
      interceptRequest.cata(
        _(url).getOrElse(super.shouldInterceptRequest(view, url)),
        super.shouldInterceptRequest(view, url))
    }

    override def shouldOverrideUrlLoading(view: webkit.WebView, url: String): Boolean =
      self.shouldOverrideUrlLoading(url)

    override def onScaleChanged(view: webkit.WebView, o: Float, n: Float): Unit = {
      scale.attemptFire(n)
    }
  }
  super.setWebViewClient(client)

  val chrome = new android.webkit.WebChromeClient {
    override def onConsoleMessage(message: webkit.ConsoleMessage): Boolean = {
      val source = if(message.sourceId() == null)
        "UNKNOWN"
      else if (message.sourceId.contains(":/"))
        message.sourceId.urlDecoded
      else
        message.sourceId
      message.messageLevel match {
        case webkit.ConsoleMessage.MessageLevel.ERROR => $e(source + ":" + message.lineNumber() + ": " + message.message())
        case webkit.ConsoleMessage.MessageLevel.WARNING => $w(source + ":" + message.lineNumber() + ": " + message.message())
        case _ => $d(source + ":" + message.lineNumber() + ": " + message.message())
      }
      true
    }
  }
  super.setWebChromeClient(chrome)

  setDownloadListener(new webkit.DownloadListener {
    def onDownloadStart(url: String, userAgent: String, contentDisposition: String, mimetype: String, contentLength: Long): Unit = {
      $w(s"DOWNLOAD url=$url mimetype=$mimetype")
    }
  })

  def loadUri(uri: java.net.URI): Promise[java.net.URI] = {
    warnIfNotOnUiThread
    val promise = Promise.emptyPromise[java.net.URI]
    synchronized {
      loadUriPromise.foreach(_.break)
      loadUriPromise = Some(promise)
      \/.fromTryCatch {
        loadUrl(uri.toString)
      }.fold({ t =>
        t.printStackTrace
        loadUriPromise = None
        promise.break
      }, (_ => ()))
      promise
    }
  }

  def reloadP(): Promise[java.net.URI] = {
    warnIfNotOnUiThread
    val promise = Promise.emptyPromise[java.net.URI]
    synchronized {
      loadUriPromise.foreach(_.break)
      loadUriPromise = Some(promise)
      \/.fromTryCatch {
        reload()
      }.fold({ t =>
        t.printStackTrace
        loadUriPromise = None
        promise.break
      }, (_ => ()))
      promise
    }
  }

  def runJS(js: String): Unit = {
    \/.fromTryCatch {
      val script = s"javascript:(function(){$js;})()"
      loadUrl(script)
    }.fold(_.printStackTrace, _ => ())
  }

  def callJS(call: String): Unit = {
    val callWithSemi = if (call.endsWith(";")) call else call + ";"
    val func = call.replaceFirst("\\s*\\(.*$", "")
    runJS(s"if(typeof($func)=='function') $callWithSemi else console.warn('$func not defined');")
  }
}
