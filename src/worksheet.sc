val a = Array(1, 2)

val r1 = (a, a)
//Update das erste Element im ersten Array in r1 (aka "a") => die Elemente  a zugewiesen ist verändert sich
r1._1(0) = 2
//Damit verändern sich beide Elemente in r1
println("(" + r1._1.mkString(",") + ")", "(" + r1._2.mkString(",") + ")")


//Variante 2: Ersetze a mit seinem Wert
val r2 = (Array(1, 2), Array(1, 2))
//Update das erste Element im ersten Array in r2
r2._1(0) = 2
//Nur das erste Array in r2 ändert sich
println("(" + r2._1.mkString(",") + ")", "(" + r2._2.mkString(",") + ")")

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

def f(a: Int, b: Int): Int = a + b
def g(a: Int)(b: Int): Int = a + b

curry(f)(1)(1) == f(1, 1)
curry(f)(1)(1) == g(1)(1)


uncurry(g)(1, 1) == g(1)(1)
uncurry(g)(1, 1) == f(1, 1)

//val c = curry((a:Int, b:Int) => a == b)
//println("1 == 2? ", c(1)(2))
//println("2 == 2? ", c(2)(2))
//
//val c_partial = c(1)
//
//println("[partial] 1 == 2? ", c_partial(2))
//println("[partial] 1 == 1? ", c_partial(1))
