package cyborg.widget

import android.os.SystemClock
import android.webkit
import android.webkit.{DownloadListener, ConsoleMessage, MimeTypeMap}
import cyborg.Context._
import cyborg.Log._
import cyborg.net.URI
import cyborg.util.control._
import cyborg.util.execution._
import cyborg.util.io._
import java.io.{IOException, ByteArrayOutputStream}
import java.net.URLDecoder
import scala.collection.JavaConversions._
import scala.util.control.Exception._
import scalaz._, Scalaz._
import scala.collection.mutable

class WebView()(implicit val context: Context) extends android.webkit.WebView(context) {
  val chromeClient: WebChromeClient = new WebChromeClient
  super.setWebChromeClient(chromeClient)

  setDownloadListener(new DownloadListener {
    def onDownloadStart(url: String, userAgent: String, contentDisposition: String, mimetype: String, contentLength: Long): Unit = {
      $w(s"DOWNLOAD url=$url mimetype=$mimetype")
    }
  })

  val webViewBitchHandler = handling(classOf[NullPointerException]) by { (ex) =>
    $d("WebView is being a little bitch again")
  }

  def setWebChromeClient(client: WebChromeClient): Unit = {
    throw new UnsupportedOperationException("The chrome client cannot be changed")
  }

  @deprecated("Use URI parameter") def safeLoadUrl(url: String): Unit = {
    if (!cyborg.Activity.inUiThread) $e("ERROR this should only be called from the UI thread: " + cyborg.util.debug.getStackTrace)
    WebView.checkUrl(url)
    webViewBitchHandler {
      loadUrl(url)
    }
  }

  def safeLoadUrl(url: URI): Unit = {
    if (!cyborg.Activity.inUiThread) $e("ERROR this should only be called from the UI thread: " + cyborg.util.debug.getStackTrace)
    WebView.checkUrl(url)
    webViewBitchHandler {
      loadUrl(url.toString)
    }
  }

  @deprecated("Use URI parameter") def safeLoadUrl(url: String, httpHeaders: Map[String, String]): Unit = {
    if (!cyborg.Activity.inUiThread) $e("ERROR this should only be called from the UI thread: " + cyborg.util.debug.getStackTrace)
    WebView.checkUrl(url)
    webViewBitchHandler {
      loadUrl(url, httpHeaders)
    }
  }

  def safeLoadUrl(url: URI, httpHeaders: Map[String, String]): Unit = {
    if (!cyborg.Activity.inUiThread) $e("ERROR this should only be called from the UI thread: " + cyborg.util.debug.getStackTrace)
    WebView.checkUrl(url)
    webViewBitchHandler {
      loadUrl(url.toString, httpHeaders)
    }
  }

  def runJS(js: String): Unit = {
    handling(classOf[Exception]) by { (ex) =>
      $e(s"Exception during runJS: $ex")
      ex.printStackTrace()
    } apply {
      val script = s"javascript:(function(){$js;})()"
      chromeClient.lastRunJS = Some(js)
      chromeClient.lastRunTime = Some(SystemClock.uptimeMillis())
      runOnMainLooper {
        safeLoadUrl(script)
      }
    }
  }

  def callJS(call: String): Unit = {
    val callWithSemi = if (call.endsWith(";")) call else call + ";"
    val func = call.replaceFirst("\\s*\\(.*$", "")
    //$i(callWithSemi)
    runJS(s"if(typeof($func)=='function') $callWithSemi else console.warn('$func not defined');")
  }

  def loadJS(filename: String): Unit = {
    $w("This should not be used from Android 4.4 on.")
    catching(classOf[IOException]).opt {
      val in = getContext.getAssets.open(filename)
      val buffer = new ByteArrayOutputStream()
      buffer << "javascript:"
      val bytes = inStream2outStream(in, buffer)
      //$d(s"Loaded $bytes bytes from '$filename'")
      in.close()
      val string = buffer.toString("UTF-8")
      chromeClient.lastRunJS = Some(string)
      chromeClient.lastRunTime = Some(SystemClock.uptimeMillis())
      string
    } map safeLoadUrl
  }

  def loadCSS(url: String): Unit = {
    runJS(s"""$$("head").append("<link rel='stylesheet' type='text/css' href='$url'/>");""")
  }

  def url = getUrl
  def urlPath: Option[String] = URI(getUrl).scheme.map(URLDecoder.decode(_, "UTF-8"))
  def urlQuery: Option[String] = URI(getUrl).query.map(URLDecoder.decode(_, "UTF-8"))
  def urlFragment: Option[String] = URI(getUrl).fragment.map(URLDecoder.decode(_, "UTF-8"))
  def urlDecoded: String = URLDecoder.decode(url, "UTF-8")

  def mimeType: Option[String] = {
    urlPath.map { (path) =>
      MimeTypeMap.getSingleton.getMimeTypeFromExtension(
        MimeTypeMap.getFileExtensionFromUrl(path))
    }
  }

  class WebChromeClient extends android.webkit.WebChromeClient {
    var lastRunJS: Option[String] = None
    var lastRunTime: Option[Long] = None
    var sources: mutable.HashMap[String, List[String]] = mutable.HashMap.empty

    override def onConsoleMessage(message: ConsoleMessage): Boolean = {
      val source = if(message.sourceId() == null)
        "UNKNOWN"
      else if (message.sourceId contains ":/")
        URLDecoder.decode(message.sourceId)
      else
        message.sourceId

      message.messageLevel match {
        case ConsoleMessage.MessageLevel.ERROR => $e(source + ":" + message.lineNumber() + ": " + message.message())
        case ConsoleMessage.MessageLevel.WARNING => $w(source + ":" + message.lineNumber() + ": " + message.message())
        case _ => $d(source + ":" + message.lineNumber() + ": " + message.message())
      }

      if (message.messageLevel == ConsoleMessage.MessageLevel.ERROR ||
        message.messageLevel == ConsoleMessage.MessageLevel.WARNING) {

        if (message.sourceId() == null || message.sourceId().isEmpty) {
          for (js <- lastRunJS; time <- lastRunTime) {
            if (message.lineNumber() <= js.lines.size && math.abs(SystemClock.uptimeMillis() - time) < 1000) {
              $d("Possibly for this code:\n" + js)
            }
          }
        }
        /*else if(sources.contains(message.sourceId) && (message.lineNumber-1) < sources(message.sourceId).size) {
          $d(source + ":" + message.lineNumber() + ": " + sources(message.sourceId)(message.lineNumber-1))
        }*/
      }

      true
    }
  }
}

object WebView {
  def escapeJSString(str: String): String = {
    if (str == null) return "null"
    else return "\"" + str.replace("\\","\\\\")
                          .replace("\n","\\n")
                          .replace("\b","\\b")
                          .replace("\r","\\r")
                          .replace("\t","\\t")
                          .replace("\'","\\'")
                          .replace("\f","\\f")
                          .replace("\"","\\\"") + "\""
  }

  val NoCacheHeaders = Map(
    "Pragma" -> "no-cache",
    "Cache-Control" -> "no-cache"
  )

  @deprecated("Use URI parameter") def checkUrl(url: String): Unit = {
    if (!url.startsWith("javascript:")) {
      stackTraceHandler(Nil) {
        URI(url).host.flatMap(x =>
          if (x.isEmpty) Some("Android 4.4 does not like zero length hostnames: " + url)
          else None
        ).foreach($w(_))
      }
    }
  }

  def checkUrl(uri: URI): Unit = {
    if (uri.scheme.exists(_ != "javascript")) {
      stackTraceHandler(Nil) {
        uri.host.flatMap(x =>
          if (x.isEmpty) Some("Android 4.4 does not like zero length hostnames: " + uri)
          else None
        ).foreach($w(_))
      }
    }
  }

}

class WebViewClient extends android.webkit.WebViewClient {
  private var previousUrl = "" //for debouncing
  var pageFinishedFunction: Option[() => Unit] = None

  override def onPageFinished(view: webkit.WebView, url: String): Unit = {
    super.onPageFinished(view, url)
    $d(s"PAGE finished current=$url previous=$previousUrl")
    if (url != previousUrl) {
      previousUrl = url
      pageFinishedFunction.map(_())
    }
  }

  def whenPageFinished(f: => Unit): Unit = {
    pageFinishedFunction = Some(() => f)
  }
}