package core

final case class Greeting(name: String) {
  def text: String = s"Hello, $name"
}
