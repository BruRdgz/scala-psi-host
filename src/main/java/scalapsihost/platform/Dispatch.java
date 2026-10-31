package scalapsihost.platform;

/** Answer table for generated Host_* subclasses of abstract platform classes. */
public interface Dispatch {
  Object call(String method, Object[] args);
}
