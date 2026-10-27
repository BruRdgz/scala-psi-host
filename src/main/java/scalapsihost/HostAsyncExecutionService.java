package scalapsihost;

import com.intellij.openapi.application.*;

import java.lang.reflect.Proxy;
import java.util.concurrent.Callable;
import java.util.concurrent.Executor;

/**
 * The host owns threading: a non-blocking read action runs synchronously on the caller's thread,
 * which already holds the host's snapshot. Builder calls are accepted and ignored; any other
 * terminal operation (submit to an executor, UI continuations) is unsupported.
 */
public final class HostAsyncExecutionService extends AsyncExecutionService {
  @Override protected ExpirableExecutor createExecutor(Executor executor) { return CallLog.unsupported("AsyncExecutionService.createExecutor"); }
  @Override protected AppUIExecutor createUIExecutor(ModalityState modality) { return CallLog.unsupported("AsyncExecutionService.createUIExecutor"); }
  @Override protected AppUIExecutor createWriteThreadExecutor(ModalityState modality) { return CallLog.unsupported("AsyncExecutionService.createWriteThreadExecutor"); }

  @Override
  @SuppressWarnings("unchecked")
  protected <T> NonBlockingReadAction<T> buildNonBlockingReadAction(Callable<? extends T> callable) {
    CallLog.hit("AsyncExecutionService.buildNonBlockingReadAction");
    return (NonBlockingReadAction<T>) Proxy.newProxyInstance(
      NonBlockingReadAction.class.getClassLoader(),
      new Class<?>[]{NonBlockingReadAction.class},
      (proxy, method, args) -> {
        String name = method.getName();
        if (name.equals("executeSynchronously")) {
          CallLog.hit("NonBlockingReadAction.executeSynchronously");
          return callable.call();
        }
        if (method.getReturnType() == NonBlockingReadAction.class) {
          CallLog.hit("NonBlockingReadAction." + name + " (builder, ignored)");
          return proxy;
        }
        if (name.equals("hashCode")) return System.identityHashCode(proxy);
        if (name.equals("equals")) return proxy == args[0];
        if (name.equals("toString")) return "HostNonBlockingReadAction";
        return CallLog.unsupported("NonBlockingReadAction." + name);
      });
  }
}
