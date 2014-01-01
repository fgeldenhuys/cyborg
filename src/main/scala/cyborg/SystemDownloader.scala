package cyborg

import android.app.{DownloadManager => DM}
import android.content.{IntentFilter, BroadcastReceiver}
import android.database.Cursor
import android.net.Uri
import android.os.{ParcelFileDescriptor, Environment}
import cyborg.db.SQLite._
import cyborg.Context._
import Preferences._
import cyborg.util.binary._
import cyborg.util.concurrent._
import cyborg.util.control._
import cyborg.util.io._
import java.io.File
import java.util.UUID
import scala.collection.mutable
import scala.concurrent.duration._

class SystemDownloader(
                        val onSuccess: (String) => Any,
                        val onFailure: (String) => Any
                        )(implicit context: Context) {
  import SystemDownloader._
  import cyborg.Log._

  //def this()(implicit context: Context) = this(() => {})(context)

  implicit val downloadManager = context.systemService[DM]
  val store = new Preferences("cyborg.SystemDownloader")
  val queued = mutable.Queue.empty[Download]

  val onDownloadComplete = new BroadcastReceiver {
    def onReceive(androidContext: android.content.Context, intent: android.content.Intent) {
      $d("A download has completed")
      $d("Downloads:\n" + downloads.mkString("\n"))
      checkFailed
      checkSuccessful
      bump
    }
  }

  def register() {
    context.registerReceiver(onDownloadComplete, new IntentFilter(DM.ACTION_DOWNLOAD_COMPLETE))
  }

  def unregister() {
    context.unregisterReceiver(onDownloadComplete)
  }

  def enqueue(uri: Uri, title: String, description: String, filename: String): Boolean = {
    val download = Download(
      title = title,
      uri = Some(uri),
      description = Some(description),
      filename = Some(filename))
    if (!queued.exists(_.uri.exists(_.compareTo(uri) == 0)) && // uri not already queued
        !uncompletedDownloads.exists(_.uri.exists(_.compareTo(uri) == 0))) { // not already processed
      queued.enqueue(download)
      true
    }
    else {
      $d(s"Already queued/running: '$title'")
      false
    }
  }

  def bump: Option[Download] = { // maybe move queued download to running
    if (queued.nonEmpty && runningDownloads.isEmpty) {
      val download = queued.dequeue()
      for (uri <- download.uri) yield {
        val downloadsDir = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS)
        val tmpName = UUID.randomUUID.toString + ".mpub.part"
        $d(s"Download '${download.title}' to $downloadsDir/$tmpName")
        val request = new DM.Request(uri)
          .setAllowedNetworkTypes(DM.Request.NETWORK_WIFI | DM.Request.NETWORK_MOBILE)
          .setAllowedOverRoaming(false)
          .setDestinationInExternalPublicDir(Environment.DIRECTORY_DOWNLOADS, tmpName)
          .setTitle(download.title)
        download.description map (request setDescription _)
        val id = downloadManager.enqueue(request)
        sleep(50 milliseconds)
        download.copy(id = Some(id), tmpFile = Some(downloadsDir + "/" + tmpName)).sync.saveTo(store)
      }
    }
    else None
  }

  def checkFailed: Seq[Download] = {
    for (download <- failedDownloads) yield {
      stackTraceHandler(Nil) {
        download.removeFrom(store)
        download.tmpFile map (new File(_).delete())
        download.id map (downloadManager.remove(_))
        onFailure(download.bookId getOrElse "")
      }
      download
    }
  }

  def checkSuccessful: Seq[Download] = {
    for (download <- successfulDownloads; id <- download.id) yield {
      stackTraceHandler(Nil) {
        $d(s"downloadManager.openDownloadedFile($id)")
        val tmpFile = downloadManager.openDownloadedFile(id)
        download.filename match {
          case Some(target) =>
            $d(s"Copy successful download '${download.title}' to '$target'")
            val in = new ParcelFileDescriptor.AutoCloseInputStream(tmpFile)
            val targetFile = new File(target)
            targetFile.getParentFile.mkdirs()
            inStream2NewFile(in, targetFile)
            in.close()
            onSuccess(targetFile.getAbsolutePath)
          case None =>
            $w(s"No filename specified for '${download.title}'")
        }
      }
      download.removeFrom(store)
      download.id map (downloadManager remove _)
      download
    }
  }

  def downloads: Seq[Download] = {
    stackTraceHandler(Seq.empty[Download]) {
      (for (cursor <- Option(downloadManager.query(new DM.Query))) yield {
        val result = Seq.fill(cursor.getCount) {
          val download = cursor2download(cursor)
          cursor.moveToNext()
          download map (_ updateFrom store)
        }
        assert(cursor.isAfterLast)
        cursor.close()
        result.flatten
      }) getOrElse Seq.empty
    }
  }

  def firstRunningDownload: Option[Download] = downloads.find(_.isRunning)
  def runningDownloads: Seq[Download] = downloads.filter(_.isRunning)
  def failedDownloads: Seq[Download] = downloads.filter(_.isFailed)
  def successfulDownloads: Seq[Download] = downloads.filter(_.isSuccessful)
  def uncompletedDownloads: Seq[Download] = downloads.filter { d =>
    d.isPaused || d.isPending || d.isRunning
  }
  def completedDownloads: Seq[Download] = downloads.filter { d =>
    d.isFailed || d.isSuccessful
  }

  def cancelAll() {
    queued.clear()
    for (d <- downloads; id <- d.id) {
      downloadManager remove id
    }
  }
}

object SystemDownloader {
  import Log._

  val StatusRunning = DM.STATUS_RUNNING
  val StatusFailed = DM.STATUS_FAILED
  val StatusPaused = DM.STATUS_PAUSED
  val StatusPending = DM.STATUS_PENDING
  val StatusSuccessful = DM.STATUS_SUCCESSFUL
  val StatusQueued = 1024

  case class Download(
    title: String,
    id: Option[Long] = None,
    uri: Option[Uri] = None,
    status: Int = StatusQueued,
    filename: Option[String] = None,
    tmpFile: Option[String] = None,
    description: Option[String] = None,
    size: Option[Bytes] = None,
    downloaded: Option[Bytes] = None,
    bookId: Option[String] = None
  ) {
    override def toString = s"Download($id '$title' $statusString [$uri])"

    def sync(implicit downloadManager: DM): Download = {
      for (cursor <- Option(downloadManager.query(new DM.Query))) {
        while(!cursor.isAfterLast) {
          for (check <- cursor2download(cursor)) {
            if (this.id == check.id) {
              cursor.close()
              return copy(
                status = check.status,
                title = check.title,
                description = check.description,
                size = check.size,
                downloaded = check.downloaded,
                bookId = check.bookId)
            }
          }
          cursor.moveToNext()
        }
        cursor.close()
      }
      this
    }

    def statusString = status match {
      case StatusRunning => "running"
      case StatusPending => "pending"
      case StatusPaused => "paused"
      case StatusSuccessful => "successful"
      case StatusFailed => "failed"
      case StatusQueued => "queued"
      case _ => "unknown"
    }

    def progress: Option[Double] =
      for (d <- downloaded; s <- size) yield d.toDouble / s.toDouble

    def isRunning = status == StatusRunning
    def isFailed = status == StatusFailed
    def isPaused = status == StatusPaused
    def isPending = status == StatusPending
    def isSuccessful = status == StatusSuccessful

    def saveTo(p: Preferences)(implicit context: Context): Download = {
      for (id <- this.id; uri <- this.uri) {
        val pre = s"dl-$id-"
        val edit = p.raw.edit()
        edit.putString(pre + "uri", uri.toString)
        $d("SAVING DOWNLOAD INFO: " + pre + "uri -> " + uri.toString)
        filename.map(edit.putString(pre + "filename", _))
        tmpFile.map(edit.putString(pre + "tmpfile", _))
        description.map(edit.putString(pre + "description", _))
        size.map(edit.putInt(pre + "size", _))
        bookId.map(edit.putString(pre + "bookid", _))
        edit.apply()
      }
      this
    }

    def updateFrom(p: Preferences)(implicit context: Context): Download = {
      (for (id <- this.id) yield {
        val pre = s"dl-$id-"
        var updated = this.copy()
        if (uri.isEmpty) p[String](pre + "uri").map(x => updated = updated.copy(uri = Option(Uri.parse(x))))
        if (filename.isEmpty) p[String](pre + "filename").map(x => updated = updated.copy(filename = Some(x)))
        if (tmpFile.isEmpty) p[String](pre + "tmpfile").map(x => updated = updated.copy(tmpFile = Some(x)))
        if (description.isEmpty) p[String](pre + "description").map(x => updated = updated.copy(description = Some(x)))
        if (size.isEmpty) p[Int](pre + "size").map(x => updated = updated.copy(size = Some(Bytes(x))))
        if (bookId.isEmpty) p[String](pre + "bookid").map(x => updated = updated.copy(bookId = Some(x)))
        updated
      }) getOrElse this
    }

    def removeFrom(p: Preferences)(implicit context: Context): Boolean = {
      (for (id <- this.id) yield {
        val pre = s"dl-$id-"
        val edit = p.raw.edit()
        edit.remove(pre + "uri")
        edit.remove(pre + "filename")
        edit.remove(pre + "tmpfile")
        edit.remove(pre + "description")
        edit.remove(pre + "size")
        edit.remove(pre + "bookid")
        edit.apply()
        true
      }) getOrElse false
    }
  }

  private def cursor2download(cursor: Cursor): Option[Download] = {
    for {
      id <- cursor.get[Int](DM.COLUMN_ID)
      status <- cursor.get[Int](DM.COLUMN_STATUS)
      title <- cursor.get[String](DM.COLUMN_TITLE)
    } yield {
      val description = cursor.get[String](DM.COLUMN_DESCRIPTION)
      val size = cursor.get[Int](DM.COLUMN_TOTAL_SIZE_BYTES).map(Bytes(_))
      val downloaded = cursor.get[Int](DM.COLUMN_BYTES_DOWNLOADED_SO_FAR).map(Bytes(_))
      Download(
        title = title,
        id = Some(id),
        status = status,
        description = description,
        size = size,
        downloaded = downloaded)
    }
  }
}
