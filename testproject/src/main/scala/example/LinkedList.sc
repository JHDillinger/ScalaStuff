sealed trait LinkedList[A] {
  def fold[B](end: B, pair: (A, B) => B): B =
    this match {
      case End() => end
      case Pair(hd, tl) => pair(hd, tl.fold(end, pair))
    }

  def length: Int =
    this match {
      case End() => 0
      case Pair(_, tl) => 1 + tl.length
    }

  def contains(a: A): Boolean =
    this match {
      case End() => false
      case Pair(hd, tl) => if (hd.equals(a)) true else tl.contains(a)
    }

  def apply(n: Int): Result[A] =
    this match {
      case End() => Failure("Index out of bounds")
      case Pair(hd, tl) =>
        n match {
          case 0 => Success(hd)
          case _ => tl(n - 1)
        }
    }

  def map[B](fn:A=>B): LinkedList[B] =
    this match {
      case End() => End[B]()
      case Pair(hd,tl) => Pair(fn(hd), tl.map(fn))

    }
}

final case class End[A]() extends LinkedList[A]

final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

//val list = List(1,2,3)
//list.flatMap(x => List(x, -x))

