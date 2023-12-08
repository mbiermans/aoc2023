package adventofcode
import scala.io.Source

object Day04 extends App {

    val input = Source.fromResource("day_04.txt").getLines.toList

    val regex = """Card *(\d+): (.+) \| (.+)""".r

    val state = (
        for {
            (line, index) <- input.zipWithIndex if line.nonEmpty
            (card, picked, won) = line match {
                case regex(c, p, w) => (c, p, w)
            }
            overlap = picked.split(" ")
                .filter(_.nonEmpty)
                .map(_.toInt)
                .intersect(
                    won.split(" ")
                        .filter(_.nonEmpty)
                        .map(_.toInt)
                ).toList
        } yield card.toInt -> overlap.size
    ).toMap


    def accum(lookup: Map[Int, Int])(count: Int, next: Int, toCount: List[Int]): Int = {
        if(toCount.isEmpty){
            count + 1
        } else {
            lookup.getOrElse(next, 0) match {
                case 0   => accum(lookup)(count + 1, next = toCount.head, toCount = toCount.tail) 
                case x   => {
                    val n = (1 to x)
                        .map(_ + next)
                        .toList
                    accum(lookup)(count + 1, n.head, n.tail ++ toCount)
                } 
            }
        }
    }

    val start = state.keys.toList.sorted


    val t1 = System.nanoTime();
    println(accum(state)(0, start.head, start.tail))
    val t2 = System.nanoTime()

    println((t2 - t1).toDouble / 1000000.0 + "ms") 
}
