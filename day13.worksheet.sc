import scala.io.Source
val input = Source.fromFile("day13.input.txt").getLines.toList

val points = input.collect { case s"${x},${y}" =>
  (x.toInt, y.toInt)
}.toSet

val foldInstructions = input.collect { case s"fold along ${axis}=${index}" =>
  (axis, index.toInt)
}

def fold(folds: List[(String, Int)]) = folds
  .foldLeft(points) { case (points, (axis, foldValue)) =>
    points.map((x, y) =>
      if (axis == "x") (x - (x - foldValue).max(0) * 2, y)
      else (x, y - (y - foldValue).max(0) * 2)
    )
  }

val part1 = fold(foldInstructions.slice(0, 1)).size

val finalGrid = fold(foldInstructions)

val lineSize = finalGrid.map((x, _) => x).max
val columnSize = finalGrid.map((_, y) => y).max

val part2 = {
  for (y <- 0 to columnSize; x <- 0 to lineSize)
    yield
      (if (x == 0) "\n"
       else "") + (if (finalGrid.contains(x, y)) "#" else ".")
}.reduce(_ + _)
