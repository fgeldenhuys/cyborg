package cyborg.widget

import scala.util.control.Exception._
import cyborg.Context.Context
import cyborg.Log
import cyborg.util.execution._
import cyborg.util.io._
import cyborg.net.URLExt._
import android.os.SystemClock
import java.io.{IOException, ByteArrayOutputStream}
import java.net.URLDecoder
import android.webkit.{ConsoleMessage, MimeTypeMap}
import android.webkit
import za.co.wmgc.AugmentedWebView

class WebView(implicit val context: Context) extends android.webkit.WebView(context) with Log {
  var chromeClient: Option[WebChromeClient] = None

  val webViewBitchHandler = handling(classOf[NullPointerException]) by { (ex) =>
    $d("WebView is being a little bitch again")
  }

  def setWebChromeClient(client: WebChromeClient) {
    super.setWebChromeClient(client)
    chromeClient = Option(client)
  }

  def safeLoadUrl(url: String) {
    webViewBitchHandler {
      loadUrl(url)
    }
  }

  def runJS(js: String) {
    handling(classOf[Exception]) by { (ex) =>
      $e(s"Exception during runJS: $ex")
      ex.printStackTrace()
    } apply {
      val script = s"javascript:(function(){$js;})()";
      for (cc <- chromeClient) {
        cc.lastRunJS = Some(script)
        cc.lastRunTime = Some(SystemClock.uptimeMillis())
      }
      webViewBitchHandler {
        runOnMainLooper {
          safeLoadUrl(script)
        }
      }
    }
  }

  def callJS(call: String) {
    val func = call.replaceFirst("\\s*\\(.*$", "")
    runJS(s"if(typeof($func)=='function') $call;")
  }

  def loadJS(filename: String) {
    catching(classOf[IOException]).opt {
      val in = getContext.getAssets.open(filename)
      val buffer = new ByteArrayOutputStream()
      buffer << "javascript:(function(){"
      buffer << in
      buffer << ";})()"
      buffer.toString("UTF-8")
    } map safeLoadUrl
  }

  def loadCSS(url: String) {
    runJS(s"""$$("head").append("<link rel='stylesheet' type='text/css' href='$url'/>");""")
  }

  def url = getUrl
  def urlFile: Option[String] = for (URLFile(file) <- URL(getUrl)) yield URLDecoder.decode(file, "UTF-8")
  def urlPath: Option[String] = for (URLPath(path) <- URL(getUrl)) yield URLDecoder.decode(path, "UTF-8")
  def urlQuery: Option[String] = for (URLRef(query) <- URL(getUrl)) yield URLDecoder.decode(query, "UTF-8")
  def urlRef: Option[String] = for (URLRef(ref) <- URL(getUrl)) yield URLDecoder.decode(ref, "UTF-8")
  def urlDecoded: String = URLDecoder.decode(url, "UTF-8")

  def mimeType: Option[String] = {
    urlPath.map { (path) =>
      MimeTypeMap.getSingleton.getMimeTypeFromExtension(
        MimeTypeMap.getFileExtensionFromUrl(path))
    }
  }

}

object WebView {
  def escapeJSString(str: String): String = {
    if (str == null) return "null"
    else return "\"" + str.replace("\"", "\\\"").replace("\n", "\\n") + "\""
  }
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

class WebChromeClient extends android.webkit.WebChromeClient with Log {
  var lastRunJS: Option[String] = None
  var lastRunTime: Option[Long] = None

  override def onConsoleMessage(message: ConsoleMessage): Boolean = {
    val source = if(message.sourceId() == null)
      "UNKNOWN"
    else if (message.sourceId contains ":/")
      URLDecoder.decode(AugmentedWebView.strip(message.sourceId))
    else
      message.sourceId
    message.messageLevel match {
      case ConsoleMessage.MessageLevel.ERROR => $e(source + ":" + message.lineNumber() + ": " + message.message())
      case ConsoleMessage.MessageLevel.WARNING => $w(source + ":" + message.lineNumber() + ": " + message.message())
      case _ => $d(source + ":" + message.lineNumber() + ": " + message.message())
    }
    for (js <- lastRunJS; time <- lastRunTime) {
      if (message.sourceId() == null &&
        message.lineNumber() <= js.lines.size &&
        math.abs(SystemClock.uptimeMillis() - time) < 1000) {
        $d("Possibly for this code:\n" + lastRunJS)
      }
    }
    true
  }
}