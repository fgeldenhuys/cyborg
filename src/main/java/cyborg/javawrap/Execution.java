package cyborg.javawrap;

import cyborg.util.execution;
import cyborg.util.execution$;

public class Execution {
    public static class ExecutionTimer {
        execution.ExecutionTimer timer;
        public ExecutionTimer(execution.ExecutionTimer t) {
            timer = t;
        }
        public long getDuration() {
            return timer.apply().toMillis();
        }
        public long checkpoint(String message) {
            return timer.checkpoint(message).toMillis();
        }
        public long averageCheckpoint(String message) {
            return timer.averageCheckpoint(message).toMillis();
        }
        public long silentCheckpoint() {
            return timer.silentCheckpoint().toMillis();
        }
        public String averageReport() {
            return timer.averageReport();
        }
    }

    public static ExecutionTimer startTimer(String name) {
        return new ExecutionTimer(new execution.ExecutionTimer(execution$.MODULE$.systemTime(), name));
    }
}
