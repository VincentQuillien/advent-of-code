import scala.io.Source
val input = Source.fromFile("day14.input.txt").getLines

val template = input.next
val pairs = template.dropRight(1).zip(template.tail)

val pairInsertions = input.collect { case s"$pair -> $insertion" =>
  ((pair(0), pair(1)) -> insertion(0))
}.toList

val initialPairsMap =
  pairs.groupBy(identity).view.mapValues(_.length.toLong).toMap

def polymer(steps: Int) = {
  val finalPairsMap = (1 to steps)
    .foldLeft(initialPairsMap) { case (pairsMap, _) =>
      val newPairs = pairInsertions.flatMap { case ((a, b), insertion) =>
        val occurence = pairsMap.getOrElse((a, b), 0L)
        List(
          ((a -> insertion) -> occurence),
          ((insertion -> b) -> occurence),
          ((a, b) -> -occurence)
        )
      }

      newPairs.foldLeft(pairsMap) { case (pairsMap, (pair, occurence)) =>
        pairsMap.updated(pair, pairsMap.getOrElse(pair, 0L) + occurence)
      }
    }

  val lettersOccurence = finalPairsMap.foldLeft(Map[Char, Long]()) {
    case (acc, ((a, _), occurence)) =>
      acc
        .updated(a, acc.getOrElse(a, 0L) + occurence)
  }

  lettersOccurence.values.max + 1 - lettersOccurence.values.min
}

val part1 = polymer(steps = 10)
val part2 = polymer(steps = 40)
