package cyborg.javawrap;

import cyborg.Log$;

public class Log {
    private static scala.Option<String> None = scala.Option.apply(null);

    public static void $d(String message) { Log$.MODULE$.java_$d(message); }
    public static void $i(String message) { Log$.MODULE$.java_$i(message); }
    public static void $w(String message) { Log$.MODULE$.java_$w(message); }
    public static void $e(String message) { Log$.MODULE$.java_$e(message); }

    public static void $d(String message, String tag) { Log$.MODULE$.java_$d(message, tag); }
    public static void $i(String message, String tag) { Log$.MODULE$.java_$i(message, tag); }
    public static void $w(String message, String tag) { Log$.MODULE$.java_$w(message, tag); }
    public static void $e(String message, String tag) { Log$.MODULE$.java_$e(message, tag); }

    public static void setGlobalTag(String tag) {
        Log$.MODULE$.globalTag_$eq(tag);
    }
}
