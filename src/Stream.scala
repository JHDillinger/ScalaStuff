package introFP.Stream

sealed trait Stream[+A] {
  // make methods from companion object available
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => {
      if (n > 1) cons(h(), t().take(n - 1))
      else if (n == 1) cons(h(), empty)
      else empty
    }
    case Empty => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) => if (n > 0) t().drop(n - 1) else this
    case Empty => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
    case Empty => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)


  //  Aufgabe 1: map, filter, append, flatMap
  //  Use the smart constructors from the Stream object

  def append[B >: A](b: => Stream[B]): Stream[B] = ???

  //  Aufgabe 2: takeWhile
  //  a)
  def takeWhile(p: A => Boolean): Stream[A] = ???

  //  b)
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = ???

  //  c)
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = ???

  // Aufgabe 3: tails
  def tails: Stream[Stream[A]] = ???


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  val ones: Stream[Int] = cons(1, ones) // infinite ones

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  val fibsViaUnfold =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }


}

