package core

import scala.collection.mutable
import scala.util.Try

/**
 * Counts things.
 */
class Counter(start: Int) {
  private var count: Int = start

  def increment(by: Int): Unit = {
    count += by
    count = count + 1
  }

  def current: Int = count
}

object Counter {
  def zero: Counter = new Counter(0)
}
