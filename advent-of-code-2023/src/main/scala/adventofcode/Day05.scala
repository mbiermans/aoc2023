package adventofcode

import scala.util.matching.Regex
import scala.io.Source


object Day05 extends App {
    val seedsRegex: Regex = """seeds: (.+)""".r
    val rangeRegex: Regex = """(\d+) (\d+) (\d+)""".r


    def range(input: String): Map[Long, Long] = {
        input match {
            case rangeRegex(dest, source, length) => 
                (0L until length.toInt).map(x => (x + source.toLong, x + dest.toLong)).toMap
        }
    }

    def getMappings(block: String): Map[Long, Long] = {
        (
            for {
                line <- block.split("\n").toList.tail
                r = range(line)
                _ = println(r.toList)
            } yield r
        ).foldLeft(Map.empty)(_ ++ _)
    }

    def getSeeds(input: String): List[Long] = {
        input match {
            case seedsRegex(s) => s.split(" ").map(_.toLong).toList
        }
    }

    def traverse(mapping: List[Map[Long, Long]])(next: Long): Long = {
        if(mapping.isEmpty){
            next
        } else {
            traverse(mapping.tail)(mapping.head.getOrElse(next, next))
        }
    }


    val fileName = "day_05.txt"
    val input = Source.fromResource(fileName)
        .getLines().mkString("\n").split("\n\n").toList

    val seeds = getSeeds(input.head)

    println(seeds)
    val mappings = input.tail.map(getMappings)
}