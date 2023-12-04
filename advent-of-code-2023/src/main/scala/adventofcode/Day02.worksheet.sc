import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.util.matching.Regex
import scala.io.Source

enum Colour(repr: String):
  case RED extends Colour("red")
  case GREEN extends Colour("green")
  case BLUE extends Colour("blue")

val input =  Source.fromFile("advent-of-code-2023/src/main/resources/day_02.txt").getLines.toList


val testInput = """|
               |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
               |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
               |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
               |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
               |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

val idRegex: Regex = """Game (\d+): (.+)""".r
val turnRegex: Regex = """(\d+) (\w+)""".r

val initialState = Map(
  Colour.RED -> 12,
  Colour.GREEN -> 13,
  Colour.BLUE -> 14
)

def parseLine(line: String): (String, List[List[(Colour, Int)]]) = {

  line.trim match {
    case idRegex(id, rest) => id ->
      (
        for {
          rawTurns <- rest.split(";").toList
          turns    =  rawTurns.split(", ").map(_.trim).toList
        } yield for {
          t <- turns
        } yield t match {
          case turnRegex(number, colour) =>
            Colour.valueOf(colour.toUpperCase) -> number.toInt
        }
      )
  }

}


def checkPossibility(state: Map[Colour, Int])(turns: List[(Colour, Int)]): Boolean = {

  (
      for {
      (colour, number)     <- turns
      possible             =  state.get(colour).map(x => x >= number).getOrElse(false) 
    } yield possible
  ).foldLeft(true)(_ && _)
}

def calcFewestNeeded(turns: List[List[(Colour, Int)]]): Map[Colour, Int] = {

  turns.flatten
    .groupBy{ case (colour, _) => colour}
    .map{ case (colour, numbers) => colour -> numbers.map(_._2).max
  }

}

val output1 = for {
  line            <- input if line.trim.nonEmpty
  (gameId, turns) = parseLine(line) if turns.forall(checkPossibility(initialState))
} yield gameId.toInt

output1.sum

val output2 = for {
  line            <- input if line.trim.nonEmpty
  (gameId, turns) = parseLine(line)
  fewest          = calcFewestNeeded(turns)
} yield gameId -> fewest(Colour.RED) * fewest(Colour.GREEN) * fewest(Colour.BLUE)

output2.map(_._2).sum