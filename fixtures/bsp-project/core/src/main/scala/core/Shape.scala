package core

trait Shape {
  def area: Double
}

final class Square(side: Double) extends Shape {
  def area: Double = side * side
}

final class Circle(radius: Double) extends Shape {
  def area: Double = 3.14 * radius * radius
}
