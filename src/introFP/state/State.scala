package introFP.state


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(run(_) match {
    case (a, newS) => (f(a), newS)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State((s: S) => go(s, sas, List()))
  }

  val intProg: State[Int, Int] = for {
    _ <- set[Int](3)
    _ <- modify[Int](_ + 1)
    _ <- modify[Int](_ * 3)
    _ <- modify[Int](_ - 2)
    result <- get
  } yield result


  //  Aufgabe 2:
  val stringProg = ???

  def stringProg2(str: List[String]): State[String, String] = ???

  ////

  def main(args: Array[String]): Unit = {
    println(intProg.run(3)._1)
  }

}

