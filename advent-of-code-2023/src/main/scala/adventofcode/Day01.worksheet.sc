import scala.util.matching.Regex
import scala.io.Source

val input = Source.fromFile("advent-of-code-2023/src/main/resources/day_01.txt").getLines.toList
val input = Source.fromFile("advent-of-code-2023/src/main/resources/day_01.txt").getLines.toList

val digits: Map[String, String] = (1 to 9).map(number => number.toString -> number.toString).toMap

val words: Map[String, String] = Map(
    "one"   -> "1",
    "two"   -> "2",
    "three" -> "3",
    "four"  -> "4",
    "five"  -> "5",
    "six"   -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine"  -> "9",
  )

def seek(patterns: Map[String, String])(input: String, matcher: String => (String => Boolean), reduce: String => String): String = {
  patterns
    .find { case (key, value) => matcher(input)(key) }
    .map(_._2)
    .getOrElse(seek(patterns)(reduce(input), matcher, reduce))
}

def calibrateFor(mappings: Map[String, String])(line: String): Int = {
  val first = seek(mappings)(line, _.startsWith, _.tail)
  val last  = seek(mappings)(line, _.endsWith, _.init)
  Integer.parseInt(s"$first$last")
}

val calibrationValues = input.filter(_.nonEmpty)

val output1 = calibrationValues.map(calibrateFor(digits)).sum
val output2 = calibrationValues.map(calibrateFor(digits ++ words)).sum
