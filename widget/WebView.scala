package cyborg.widget

import android.os.SystemClock
import android.webkit
import android.webkit.{ConsoleMessage, MimeTypeMap}
import cyborg.Context._
import cyborg.Log
import cyborg.net.URIExt._
import cyborg.util.execution._
import cyborg.util.io._
import java.io.{IOException, ByteArrayOutputStream}
import java.net.URLDecoder
import scala.collection.JavaConversions._
import scala.util.control.Exception._

class WebView()(implicit val context: Context) extends android.webkit.WebView(context) with Log {
  val chromeClient: WebChromeClient = new WebChromeClient
  super.setWebChromeClient(chromeClient)

  val webViewBitchHandler = handling(classOf[NullPointerException]) by { (ex) =>
    $d("WebView is being a little bitch again")
  }

  def setWebChromeClient(client: WebChromeClient) {
    throw new UnsupportedOperationException("The chrome client cannot be changed")
  }

  def safeLoadUrl(url: String) {
    webViewBitchHandler {
      loadUrl(url)
    }
  }

  def safeLoadUrl(url: String, httpHeaders: Map[String, String]) {
    webViewBitchHandler {
      loadUrl(url, httpHeaders)
    }
  }

  def runJS(js: String) {
    handling(classOf[Exception]) by { (ex) =>
      $e(s"Exception during runJS: $ex")
      ex.printStackTrace()
    } apply {
      val script = s"javascript:(function(){$js;}())"
      chromeClient.lastRunJS = Some(js)
      chromeClient.lastRunTime = Some(SystemClock.uptimeMillis())
      webViewBitchHandler {
        runOnMainLooper {
          safeLoadUrl(script)
        }
      }
    }
  }

  def callJS(call: String) {
    val callWithSemi = if (call.endsWith(";")) call else call + ";"
    val func = call.replaceFirst("\\s*\\(.*$", "")
    runJS(s"if(typeof($func)=='function') $callWithSemi else console.warn('$func not defined');")
  }

  def loadJS(filename: String) {
    catching(classOf[IOException]).opt {
      val in = getContext.getAssets.open(filename)
      val buffer = new ByteArrayOutputStream()
      buffer << "javascript:"
      val bytes = inStream2outStream(in, buffer)
      //$d(s"Loaded $bytes bytes from '$filename'")
      val string = buffer.toString("UTF-8")
      //$d(s"Converted to string of ${string.size} characters:")
      chromeClient.lastRunJS = Some(string)
      chromeClient.lastRunTime = Some(SystemClock.uptimeMillis())
      string
    } map safeLoadUrl
  }

  def loadCSS(url: String) {
    runJS(s"""$$("head").append("<link rel='stylesheet' type='text/css' href='$url'/>");""")
  }

  def url = getUrl
  def urlPath: Option[String] = for (URIPath(path) <- URI(getUrl)) yield URLDecoder.decode(path, "UTF-8")
  def urlQuery: Option[String] = for (URIQuery(query) <- URI(getUrl)) yield URLDecoder.decode(query, "UTF-8")
  def urlFragment: Option[String] = for (URIFragment(ref) <- URI(getUrl)) yield URLDecoder.decode(ref, "UTF-8")
  def urlDecoded: String = URLDecoder.decode(url, "UTF-8")

  def mimeType: Option[String] = {
    urlPath.map { (path) =>
      MimeTypeMap.getSingleton.getMimeTypeFromExtension(
        MimeTypeMap.getFileExtensionFromUrl(path))
    }
  }

  class WebChromeClient extends android.webkit.WebChromeClient with Log {
    var lastRunJS: Option[String] = None
    var lastRunTime: Option[Long] = None

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
        for (js <- lastRunJS; time <- lastRunTime) {
          if (message.sourceId() == null &&
            message.lineNumber() <= js.lines.size &&
            math.abs(SystemClock.uptimeMillis() - time) < 1000) {
            $d("Possibly for this code:\n" + js)
          }
        }
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
}

class WebViewClient extends android.webkit.WebViewClient {
  private var previousUrl = "" //for debouncing
  var pageFinishedFunction: Option[() => Unit] = None

  override def onPageFinished(view: webkit.WebView, url: String) {
    super.onPageFinished(view, url)
    if (url != previousUrl) {
      previousUrl = url
      pageFinishedFunction.map(_())
    }
  }

  def whenPageFinished(f: => Unit) {
    pageFinishedFunction = Some(() => f)
  }
}
