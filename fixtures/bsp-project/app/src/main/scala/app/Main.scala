package app

import core.*

object Main {
  def main(args: Array[String]): Unit = {
    val text: String = Shout.loud(Greeting("world").text)
    println(text)
  }
}
