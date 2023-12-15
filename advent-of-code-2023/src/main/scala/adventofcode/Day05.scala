package adventofcode

import scala.util.matching.Regex
import scala.io.Source

case class MappingRange(
    lowerBound: Long,
    upperBound: Long
) {
    def containsElement(element: Long): Boolean = {
        return lowerBound <= element && upperBound >= element
    }
}

object Day05 extends App {
  val seedsRegex: Regex = """seeds: (.+)""".r
  val rangeRegex: Regex = """(\d+) (\d+) (\d+)""".r

  def range(input: String): (Long, Long, Long) = {
    input match {
      case rangeRegex(dest, source, length) =>
        (source.toLong, dest.toLong, length.toLong)
    }
  }

  def getMappings(block: String): List[(Long, Long, Long)] = {
    (
      for {
        line <- block.split("\n").toList.tail
        r = range(line)
      } yield r
    ).foldLeft(List.empty)(_ :+ _)
  }

  def getSeeds(input: String): List[Long] = {
    input match {
      case seedsRegex(s) => s.split(" ").map(_.toLong).toList
    }
  }

  def traverse(mapping: List[List[(Long, Long, Long)]])(next: Long): Long = {
    if (mapping.isEmpty) {
      next
    } else {
      val found = mapping.head
        .find { case (source, dest, length) =>
          (source <= next) && (next < source + length)
        }
        .map {
          case (source, dest, length) => {
            val difference = next - source
            dest + difference
          }
        }
        .getOrElse(next)
      traverse(mapping.tail)(found)
    }
  }

  val fileName = "day_05.txt"
  val input = Source
    .fromResource(fileName)
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .toList

  val seeds = getSeeds(input.head)

  val mappings = input.tail.map(getMappings)

  val result = seeds.map(Day05.traverse(mappings)).min

  val seeds2 = seeds.sliding(2).map {
    case List(start, length) => {
      val range = (start until (length + start))
      range.min -> range.max
    }
  }

  def findOverlappingRanges(input: MappingRange, comparison: MappingRange): (Option[MappingRange], List[MappingRange]) = {
    val ranges = List(
        input,
        MappingRange(input.lowerBound, comparison.upperBound),
        MappingRange(comparison.lowerBound, input.upperBound)
    )

    val compContainsInput = ranges.find{
        case MappingRange(low, up) => input.containsElement(low) && input.containsElement(up) && comparison.containsElement(low) && comparison.containsElement(up)
    }

    find match {
        case Some(_) =>
            find -> ranges.filter{
                        case MappingRange(low, up) => input.containsElement(low) && input.containsElement(up) && !comparison.containsElement(low) && !comparison.containsElement(up)
                    }
        case None => 
            None -> List(input) 
    }
  }

}
