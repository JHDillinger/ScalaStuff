sealed trait TrafficLight{
  def next:TrafficLight =
    this match {
      case Red => Green
      case Green => Yellow
      case Yellow => Red
    }
}
final case object Red extends TrafficLight
final case object Green extends TrafficLight
final case object Yellow extends TrafficLight


sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(reason: String) extends Calculation

object Calculator{
  def +(calc:Calculation, i:Int):Calculation =
    calc match {
      case Success(x) => Success(i+x)
      case Failure(reason) => Failure(reason)
    }
  def -(calc:Calculation, i:Int):Calculation =
    calc match {
      case Success(x) => Success(x-i)
      case Failure(reason) => Failure(reason)
    }

  def /(calc:Calculation, operand:Int):Calculation = {
    calc match {
      case Success(result) =>
        operand match {
          case 0 => Failure("Division by zero")
          case _ => Success(result/operand)
        }
      case Failure(reason) => Failure(reason)
    }
  }
}

assert(Calculator./(Success(4), 2) == Success(2))
assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))