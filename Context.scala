package cyborg

object Context {
  //implicit class Context(val context: android.content.Context) extends AnyVal {}
  class Context(val context: android.content.Context)
  implicit def android2cyborgContext(context: android.content.Context) = new Context(context)
  implicit def cyborg2androidContext(context: Context) = context.context
}
