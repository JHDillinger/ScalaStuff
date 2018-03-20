package testpackage


object Levenshtein {

  def levenshtein(lhs: String, rhs: String): Int = {
    val lhsi = lhs.length
    val rhsi = rhs.length

    def helper(lhs: String, rhs: String, lhsi: Int, rhsi: Int): Int = {
      (lhsi, rhsi) match {
        case (0, _) => rhsi
        case (_, 0) => lhsi
        case (_, _) => {
          val cost = if (lhs(lhsi - 1) == rhs(rhsi - 1)) 0 else 1
          Math.min(
            helper(lhs, rhs, lhsi - 1, rhsi) + 1,
            Math.min(helper(lhs, rhs, lhsi, rhsi - 1) + 1,
              helper(lhs, rhs, lhsi - 1, rhsi - 1) + cost)
          )
        }
      }
    }

    helper(lhs, rhs, lhsi, rhsi)
  }


  def main(args: Array[String]): Unit = {
    val test = levenshtein("yyyyyyyyyyy", "xxxyxxxx")
    print(test)

  }

}
