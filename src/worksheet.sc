// `List` data type, parameterized on a type, `A`
sealed trait List[+A]
// A `List` data constructor representing the empty list
case object Nil extends List[Nothing]
// Another data constructor, representing nonempty lists
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// empty list of doubles
val ex1: List[Double] = Nil
// list with an int
val ex2: List[Int] = Cons(1, Nil)
// list with two strings
val ex3 = Cons("a", Cons("b", Nil))

println(ex1)
println(ex2)
println(ex3)

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}
def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case Cons(0.0, _) => 0.0
  case Cons(x,xs) => x * product(xs)
}
def apply[A](as: A*): List[A] = // Variadic function syntax
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))

println(sum(ex2))