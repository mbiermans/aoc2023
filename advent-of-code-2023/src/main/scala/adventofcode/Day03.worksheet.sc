import scala.io.Source

val testInput = Source.fromFile("advent-of-code-2023/src/main/resources/test_day_03.txt").getLines.toList
val input = Source.fromFile("advent-of-code-2023/src/main/resources/day_03.txt").getLines.toList


def makeMatrix(lines: List[String]): Map[(Int, Int), Char] = {
    (
        for {
            (line, yCoord) <- lines.zipWithIndex
            (char, xCoord) <- line.zipWithIndex
        } yield (xCoord, yCoord) -> char
    ).toMap
}


def positions(line: String) = {

    val res = line.zipWithIndex.foldLeft((List.empty[(String, List[Int])], Option.empty[(String, List[Int])])) {
        case ((words, None), (char, _)) if !char.isDigit=> 
            (words, None)
        case ((words, None), (char, pos)) if char.isDigit=> 
            (words, Some(char.toString -> List(pos)))
        case ((words, Some((accum, positions))), (char, pos)) if char.isDigit => 
            (words, Some((accum + char) -> (positions :+ pos)))
        case ((words, Some((accum, positions))), (char, _)) if !char.isDigit => 
            (words :+ (accum -> positions)) -> None
    }
    
    res match {
        case (all, Some(last)) => all :+ last
        case _ => res._1
    }
}

def getNeighbours(x: Int, y: Int): List[(Int, Int)] = {
    for {
        xMove <- (-1 to 1).toList
        yMove <- (-1 to 1).toList
    } yield Math.max(0, x + xMove) -> Math.max(0, y + yMove)
}

def isSymbol(matrix: Map[(Int, Int), Char])(x: Int, y: Int): Boolean = {
    matrix.get(x, y) match {
        case None                             => false
        case Some(x) if x.isDigit || x == '.' => false
        case Some(_)                          => true
    }
}

val testMatrix = makeMatrix(testInput)
val matrix = makeMatrix(input)

val result = for {
    (line, yCoord)      <- input.zipWithIndex
    (numbers, xCoords)  <- positions(line)
    coordinates         =  xCoords.map(x => x -> yCoord)
    neighbours          =  coordinates.flatMap(getNeighbours).distinct.diff(coordinates) if neighbours.exists(isSymbol(matrix))
} yield Integer.parseInt(numbers)



val numberMatrix = (for {
    (line, yCoord)     <- input.zipWithIndex
    (numbers, xCoords) <- positions(line)
    coord              <- xCoords.map(x => x -> yCoord)
} yield coord -> Integer.parseInt(numbers)).toMap

val result2 = for {
    (line, yCoord)     <- input.zipWithIndex
    (char, xCoord)     <- line.zipWithIndex if char == '*'
    neighbours         =  getNeighbours(xCoord, yCoord).map(numberMatrix.get).flatten.distinct if neighbours.size > 1
} yield neighbours.head * neighbours.last

result2.sum