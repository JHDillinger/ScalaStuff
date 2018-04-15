import java.util.regex.Matcher
import java.util.regex.Pattern


object RT {
  def main(args: Array[String]): Unit = {
    val m = Pattern.compile("\\d\\d").matcher("a34sd39f")

    println(m.find())
    val g = m.group()

    println(g)
    println(m.group())

    println(m.find())
    println(m.group())
  }


}
