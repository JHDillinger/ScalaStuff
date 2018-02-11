sealed trait Expression{
  def eval:Double =
    this match {
      case Number(v) => v
      case Subtraction(l,r) => l.eval - r.eval
      case Addition(l,r) => l.eval + r.eval
      case Division(l,r) => l.eval / r.eval
      case SquareRoot(v) => Math.sqrt(v.eval)
    }
}




final case class Addition(left:Expression, right: Expression) extends Expression
final case class Subtraction(left:Expression, right: Expression) extends Expression
final case class Number(v:Double) extends Expression
final case class Division(num:Expression, den:Expression) extends Expression
final case class SquareRoot(v:Expression) extends Expression

sealed trait Result
final case class Success(result: Double) extends Result
final case class Failure(reason: String) extends Result
