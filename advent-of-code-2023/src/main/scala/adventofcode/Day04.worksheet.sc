import scala.io.Source

val testInput = Source.fromFile("advent-of-code-2023/src/main/resources/test_day_04.txt").getLines.toList
val input = Source.fromFile("advent-of-code-2023/src/main/resources/day_04.txt").getLines.toList

val regex = """Card *(\d+): (.+) \| (.+)""".r


val state = (
    for {
        (line, index) <- testInput.zipWithIndex if line.nonEmpty
        (card, picked, won) = line match {
            case regex(c, p, w) => (c, p, w)
        }
        overlap = picked.split(" ").filter(_.nonEmpty).map(_.toInt).intersect(won.split(" ").filter(_.nonEmpty).map(_.toInt)).toList
    } yield card.toInt -> overlap.size
).toMap



def accum(lookup: Map[Int, Int])(count: Int, next: Int, toCount: List[Int]): Int = {
    if(toCount.isEmpty){
        count + 1
    } else {
        lookup(next) match {
            case 0      => accum(lookup)(count + 1, next = toCount.head, toCount = toCount.tail) 
            case x: Int => {
                val n = ((x + 1) to x + lookup(x)).toList
                accum(lookup)(count + 1, n.head, n.tail ++ toCount)
            } 
        }
    }
}

val start = (1 to 2).toList

"Card  10: 27 59 55 95  4 99  8 48 97  6 | 51 50 48 91 34  4 97 86 27 99 20 95 17  6 80 43 21 60 28 10 89 59 73 55  8" match {
    case regex(id, left, right) => (id, left, right)
}