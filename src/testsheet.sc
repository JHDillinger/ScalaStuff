import java.util.regex.Pattern

// Referential Transparency
// In der Vorlesung wurde anhand eines Beispiels mit StringBuilderg ezeigt, wie Nebeneffekte Referential Transparency verletzen.
// Überlegen Sie sich ein anderes, einfaches Beispiel in dem Referential Transparency nicht gegeben ist.

// Zunächst:
val m = Pattern.compile("\\d\\d").matcher("a34sd39f")
println(m.find()) //true
println(m.group()) //34

println(m.find()) //true
println(m.group()) //39


// Auslagern von m.group()
val m2 = Pattern.compile("\\d\\d").matcher("a34sd39f")
println(m2.find()) //true

val grp = m2.group()
println(grp) //34
println(m2.find()) //true
println(grp) //39




// Fibonacci
// Schreiben Sie eine rekursive Funktion, die die n-te Fibonacci Zahl berechnet.
// Die Implementierung sollte eine lokale tail-rekursive Funktion verwenden.

def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, prev: Int, curr: Int): Int =
    if (n == 0) prev
    else go(n - 1, curr, prev + curr)

  go(n, 0, 1)
}


// Higher Order Functions
// Implementieren Sie die polymorphe Funktion isSorted, die überprüft ob ein Array[A]
// nach einer übergebenen Vergleichsfunktion sortiert ist.

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(i: Int): Boolean =
    if (i >= as.length - 1) true
    else if (gt(as(i), as(i + 1))) loop(i + 1)
    else false

  loop(0)
}


// Currying
// Gegeben sind die beiden Funktionen f und g:
def f(a: Int, b: Int): Int = a + b
def g(a: Int)(b: Int): Int = a + b

// Überlegen Sie welche der folgenden Aussagen wahr oder falsch sind:
// curry(f)(1)(1) == f(1, 1)
// curry(f)(1)(1) == g(1)(1)
// uncurry(g)(1, 1) == g(1)(1)
// uncurry(g)(1, 1) == f(1, 1)