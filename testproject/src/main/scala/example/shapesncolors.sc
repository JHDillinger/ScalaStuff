//package example

/*import java.util.Date

trait Visitor {
  def id: String

  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime
}

case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

case class User(
                 id: String,
                 email: String,
                 createdAt: Date = new Date()
               ) extends Visitor

val test = Anonymous ("asdf")

test.age*/

sealed trait Color {
  def r: Int

  def g: Int

  def b: Int

}

object Red extends Color {
  val r = 255
  val g = 0
  val b = 0

}

object Yellow extends Color {
  val r = 255
  val g = 255
  val b = 0
}

object Pink extends Color {
  val r = 255
  val g = 255
  val b = 147
}

final case class CustomColor(r: Int, g: Int, b: Int) extends Color{
  override def toString: String = s"R=$r,G=$g,B=$b"
}


sealed trait Shape {
  def sides: Int

  def perimeter: Double

  def area: Double

  def color: Color

}

case class Circle(r: Double, color: Color) extends Shape {
  val sides = 1
  val perimeter = 2 * math.Pi * r
  val area = math.Pi * r * r
}

sealed trait Rectangular extends Shape {
  def width: Double

  def height: Double

  val sides = 4
  val perimeter = 2 * width + 2 * height
  val area = width * height
}


case class Rectangle(width: Double, height: Double, color: Color)
  extends Rectangular

case class Square(size: Double, color: Color) extends Rectangular {
  val width = size
  val height = size
}

object Draw {
  def apply(shape: Shape): String =
    shape match {
      case Circle(r, color)
      => s"A ${Draw(color)} circle of radius $r cm"
      case Rectangle(x, y, color)
      => s"A ${Draw(color)} rectangle of width $x and height $y"
      case Square(s, color)
      => s"A ${Draw(color)} square of side length $s"
      case _
      => "Some other shape"
    }

  def apply(color: Color): String =
    color match {
      case Red => "red"
      case Yellow => "yellow"
      case Pink => "pink"
      case _ => s"custom color (${color.toString})"
    }
}

Draw(Circle(10.0, Red))
Draw(Circle(10.0, Pink))
// returns "A pink circle of radius 10.0cm"
Draw(Rectangle(3.0, 4.0, CustomColor(123, 123, 123)))
// returns "A dark rectangle of width 3.0cm and height 4.0cm"
