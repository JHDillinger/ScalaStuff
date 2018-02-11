sealed trait Maybe[+A] {
  def fold[B](e: B, f: A => B): B =
    this match {
      case Empty => e
      case Full(a) => f(a)
    }

  def flatMap[B](fn: A=> Maybe[B]): Maybe[B] =
    this match {
      case Empty => Empty[B]()
      case Full(v) => fn(v)
    }

  def map[B](f: A => B): Maybe[B] =
    this match{
      case Empty => Empty[B]()
      case Full(v) => Full(f(v))
    }
}

final case class Full[A](value: A) extends Maybe[A]

final case object Empty extends Maybe[Nothing]

val list = List(Full(3), Full(2), Full(1))
list.map(x => if(x.value % 2 ==0) x else Empty)
//list.map(maybe => maybe flatMap { x => if(x % 2 == 0) Full(x) else Empty})