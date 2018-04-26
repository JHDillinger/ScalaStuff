//import introFP.stream._

object Main {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty
    }
  }

  def next(n: Double)(x: Double): Double = (x + n / x) / 2

  //    def repeat[A](f: A => A)(a: A): Seq[A] = a +: repeat(f)(f(a))
  def repeat[A](f: A => A)(a: A): Stream[A] = Stream.cons(a, repeat(f)(f(a)))
//
  def repeat2[A](f: A => A)(a:A):Stream[A] =
    unfold(a){n => Some((n, f(n)))}

  @annotation.tailrec
  def within(eps: Double)(l: Seq[Double]): Double =
    (eps, l) match {
      case (_, Seq()) => 0
      case (_, Seq(x)) => x
      case (_, a +: b +: rest) => {
        if (Math.abs(a - b) <= eps) b
        else within(eps)(b +: rest)
      }
    }

  @annotation.tailrec
  def relative(eps: Double)(l: Seq[Double]): Double =
    (eps, l) match {
      case (_, Seq()) => 0
      case (_, Seq(x)) => x
      case (_, a +: b +: rest) => {
        if (Math.abs(a / b - 1) <= eps) b
        else relative(eps)(b +: rest)
      }
    }


  def sqroot(a: Double, eps: Double, n: Double): Double = within(eps)(repeat[Double](next(n))(a))

  def relativesqroot(a: Double, eps: Double, n: Double): Double = relative(eps)(repeat[Double](next(n))(a))


  def main(args: Array[String]): Unit = {
    val test = sqroot(4, 0.001, 20)
    println(test)

  }
}