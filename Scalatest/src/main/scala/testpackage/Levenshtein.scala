package testpackage

import scala.annotation.tailrec


object Levenshtein {

  def lev(lhs: String, rhs: String): Int = {
    def helper(lhs: String, rhs: String, lhsi: Int, rhsi: Int): Int = {
      (lhsi, rhsi) match {
        case (0, r) => r
        case (l, 0) => l
        case (_, _) => {
          lazy val cost = if (lhs(lhsi - 1) == rhs(rhsi - 1)) 0 else 1
          min(
            helper(lhs, rhs, lhsi - 1, rhsi) + 1,
            helper(lhs, rhs, lhsi, rhsi - 1) + 1,
            helper(lhs, rhs, lhsi - 1, rhsi - 1) + cost
          )
        }
      }
    }

    helper(lhs, rhs, lhs.length, rhs.length)
  }

  def levImperative(str1: String, str2: String): Int = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1

      d(i)(j) = min(
        d(i - 1)(j) + 1, // deletion
        d(i)(j - 1) + 1, // insertion
        d(i - 1)(j - 1) + cost // substitution
      )
    }

    d(lenStr1)(lenStr2)
  }


  def levHaskell(s: String, t: String): Int = {
    lazy val d = {
      for {
        i <- 0 to 3
      } yield for {
        j <- 0 to 3
      } yield distance(i, j)
    }
    println(d)

    def distance(i: Int, j: Int): Int =
      (i, j) match {
        case (i, 0) => i
        case (0, j) => j
        case (i, j) => {
          //          Stream(d(i-1)(j)+1, d(i)(j-1), d(i-1)(j-1)+ (if (s(i-1)==t(j-1)) 0 else 1) ).min
          i + j
        }
      }

    d(s.length - 2)(t.length - 2)

  }

  def min(nums: Int*): Int = nums.min

  def main(args: Array[String]): Unit = {
        val test = levHaskell("test", "tast")
        println(test)

  }

}
