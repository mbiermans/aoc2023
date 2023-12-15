import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source
import adventofcode.Day05
import org.scalatest.matchers.should.Matchers
import adventofcode.MappingRange

class Day05Test extends AnyFlatSpec with Matchers {

    trait Day05Fixture {
        val input = Source.fromResource("test_day_05.txt")
            .getLines().mkString("\n").split("\n\n").toList

        val seeds = Day05.getSeeds(input.head)
        val mappings = input.tail.map(Day05.getMappings)
    }

    it should "correctly find the mapping for the seed" in new Day05Fixture {

        seeds.map(Day05.traverse(mappings)) shouldBe List(
            82L, 43L, 86L, 35L
        )
    }

    it should "find overlapping ranges" in new Day05Fixture {
        Day05.findOverlappingRanges(MappingRange(1L, 10L), MappingRange(2L, 8L)) shouldBe Some(MappingRange(2L, 8L)) -> List.empty

        Day05.findOverlappingRanges(MappingRange(10L, 20L), MappingRange(2L, 8L)) shouldBe None -> List(MappingRange(10L, 20L))
        
        // Day05.findOverlappingRanges(MappingRange(1L, 6L), MappingRange(2L, 8L)) shouldBe Some(MappingRange(2L, 6L))

        // Day05.findOverlappingRanges(MappingRange(6L, 20L), MappingRange(2L, 8L)) shouldBe Some(MappingRange(6L, 8L))
    }

}